#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#include "mcpc/mcpc.h"
#include "firestore_client.h"
#include "mjson.h"

// Helper to run shell command and get output
// Caller must free result
static char* get_cmd_output(const char* cmd) {
    FILE* fp = popen(cmd, "r");
    if (!fp) return NULL;

    size_t size = 1024;
    size_t len = 0;
    char* buf = malloc(size);
    if (!buf) { pclose(fp); return NULL; }

    while (!feof(fp) && !ferror(fp)) {
        if (len + 512 >= size) {
            size *= 2;
            char* new_buf = realloc(buf, size);
            if (!new_buf) { free(buf); pclose(fp); return NULL; }
            buf = new_buf;
        }
        size_t r = fread(buf + len, 1, 512, fp);
        len += r;
    }
    pclose(fp);
    buf[len] = '\0';
    
    // Trim newline
    while (len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r')) {
        buf[--len] = '\0';
    }
    
    // char log_buf[2048];
    // snprintf(log_buf, sizeof(log_buf), "CMD: %s -> OUT: %s", cmd, buf);
    // log_info_c(log_buf);

    return buf;
}

static char* g_project_id = NULL;
static char* g_access_token = NULL;

static const char* get_project_id(void) {
    if (g_project_id) return g_project_id;
    
    log_info_c("Resolving project ID...");

    // 1. Try environment variable
    char* env_project = getenv("GOOGLE_CLOUD_PROJECT");
    if (env_project && strlen(env_project) > 0) {
        g_project_id = strdup(env_project);
        return g_project_id;
    }

    // 2. Try gcloud (for local dev)
    g_project_id = get_cmd_output("gcloud config get-value project 2>/dev/null");
    if (g_project_id && strlen(g_project_id) > 0) return g_project_id;

    // 3. Try metadata server
    if (g_project_id) free(g_project_id);
    g_project_id = get_cmd_output("curl -s -H \"Metadata-Flavor: Google\" http://metadata.google.internal/computeMetadata/v1/project/project-id 2>/dev/null");
    
    return g_project_id;
}

static const char* get_access_token(void) {
    if (g_access_token) { 
        free(g_access_token); 
    }
    
    log_info_c("Refreshing access token...");

    // 1. Try metadata server (Cloud Run)
    g_access_token = get_cmd_output("curl -s -H \"Metadata-Flavor: Google\" http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token 2>/dev/null");
    if (g_access_token && strlen(g_access_token) > 0 && strstr(g_access_token, "access_token")) {
        // Parse JSON for access_token
        const char* val;
        int val_len;
        if (mjson_find(g_access_token, strlen(g_access_token), "$.access_token", &val, &val_len) == MJSON_TOK_STRING) {
            char* token = malloc(val_len + 1);
            mjson_get_string(g_access_token, strlen(g_access_token), "$.access_token", token, val_len + 1);
            free(g_access_token);
            g_access_token = token;
            return g_access_token;
        }
    }

    // 2. Try gcloud (local dev)
    if (g_access_token) free(g_access_token);
    g_access_token = get_cmd_output("gcloud auth print-access-token 2>/dev/null");
    return g_access_token;
}

// Perform Firestore REST request
// Returns JSON response string (caller must free) or NULL on failure
static char* firestore_request(const char* method, const char* subpath, const char* body) {
    const char* project = get_project_id();
    if (!project || strlen(project) == 0) return NULL;
    const char* token = get_access_token();
    if (!token || strlen(token) == 0) return NULL;

    char url[1024];
    snprintf(url, sizeof(url), 
        "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents%s",
        project, subpath ? subpath : "");

    char log_msg[256];
    snprintf(log_msg, sizeof(log_msg), "Firestore %s request to %s", method, subpath ? subpath : "/");
    log_info_c(log_msg);

    // Construct curl command
    char* cmd;
    if (body) {
        char tmp_path[] = "/tmp/fs_body_XXXXXX";
        int fd = mkstemp(tmp_path);
        if (fd == -1) return NULL;
        if (write(fd, body, strlen(body)) < 0) { 
            close(fd); return NULL;
        }
        close(fd);
        
        size_t cmd_len = strlen(url) + strlen(token) + strlen(method) + strlen(tmp_path) + 200;
        cmd = malloc(cmd_len);
        snprintf(cmd, cmd_len, 
            "curl -s -X %s -H \"Authorization: Bearer %s\" -H \"Content-Type: application/json\" -d @%s \"%s\"",
            method, token, tmp_path, url);
            
        // Run
        char* res = get_cmd_output(cmd);
        
        // Cleanup
        free(cmd);
        remove(tmp_path);
        return res;
    } else {
        size_t cmd_len = strlen(url) + strlen(token) + strlen(method) + 100;
        cmd = malloc(cmd_len);
        snprintf(cmd, cmd_len, 
            "curl -s -X %s -H \"Authorization: Bearer %s\" \"%s\"",
            method, token, url);
        char* res = get_cmd_output(cmd);
        free(cmd);
        return res;
    }
}

// --- Helpers for parsing Firestore JSON ---

// Extract simple value from field
static void extract_field(const char* fields, int fields_len, const char* key, char* out_val, int max_len) {
    char path[128];
    snprintf(path, sizeof(path), "$.%s.stringValue", key);
    if (mjson_get_string(fields, fields_len, path, out_val, max_len) > 0) return;
    
    snprintf(path, sizeof(path), "$.%s.integerValue", key);
    if (mjson_get_string(fields, fields_len, path, out_val, max_len) > 0) return;
    
    snprintf(path, sizeof(path), "$.%s.doubleValue", key);
    double d;
    if (mjson_get_number(fields, fields_len, path, &d)) {
        snprintf(out_val, max_len, "%.2f", d);
        return;
    }
     
     snprintf(path, sizeof(path), "$.%s.timestampValue", key);
     if (mjson_get_string(fields, fields_len, path, out_val, max_len) > 0) return;
     
     out_val[0] = '\0';
}

static char* doc_to_json(const char* doc_json, int len) {
    log_info_c("doc_to_json called");
    // Extract name (ID)
    char name_path[256];
    if (mjson_get_string(doc_json, len, "$.name", name_path, sizeof(name_path)) <= 0) {
        log_info_c("doc_to_json: failed to get name");
        return NULL;
    }
    
    // ID is last part of name
    char* id = strrchr(name_path, '/');
    if (id) id++; else id = name_path;

    const char *fields;
    int fields_len;
    if (mjson_find(doc_json, len, "$.fields", &fields, &fields_len) != MJSON_TOK_OBJECT) {
        log_info_c("doc_to_json: failed to find fields");
        return NULL;
    }

    char name[100] = {0};
    char price[20] = "0";
    char quantity[20] = "0";
    char imgfile[100] = {0};
    
    extract_field(fields, fields_len, "name", name, sizeof(name));
    extract_field(fields, fields_len, "price", price, sizeof(price));
    extract_field(fields, fields_len, "quantity", quantity, sizeof(quantity));
    extract_field(fields, fields_len, "imgfile", imgfile, sizeof(imgfile));
    
    char* res = mjson_aprintf("{\"id\": \"%s\", \"name\": \"%s\", \"price\": %s, \"quantity\": %s, \"imgfile\": \"%s\"}",
        id, name, price, quantity, imgfile);
    
    if (res) {
        log_info_c("doc_to_json: success");
    } else {
        log_info_c("doc_to_json: mjson_aprintf failed");
    }
    return res;
}

// --- Exposed Functions ---

void impl_get_root(void *ucbr) {
    mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, 
        "Hello! This is the Cymbal Superstore Inventory API (Fortran Edition).");
}

void impl_check_db(void *ucbr) {
    const char* p = get_project_id();
    if (p && strlen(p) > 0) {
         mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Database running: true (Project: %s)", p);
    } else {
         mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Database running: false");
    }
}

void impl_get_products(void *ucbr) {
    char* resp = firestore_request("GET", "/inventory", NULL);
    if (!resp) {
        mcpc_ucbr_toolcall_add_errmsg_printf8((mcpc_ucbr_t*)ucbr, "Failed to fetch products");
        return;
    }

    // Iterate documents
    const char *docs;
    int docs_len;
    if (mjson_find(resp, strlen(resp), "$.documents", &docs, &docs_len) != MJSON_TOK_ARRAY) {
         mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "[]");
         free(resp);
         return;
    }

    size_t cap = 4096;
    size_t size = 0;
    char* out = malloc(cap);
    out[0] = '[';
    size = 1;

    int offset = 0;
    int k_off, k_len, v_off, v_len, v_type;
    int first = 1;

    while ((offset = mjson_next(docs, docs_len, offset, &k_off, &k_len, &v_off, &v_len, &v_type)) != 0) {
        char* item_json = doc_to_json(docs + v_off, v_len);
        if (item_json) {
            size_t item_len = strlen(item_json);
            if (size + item_len + 5 > cap) {
                cap *= 2;
                out = realloc(out, cap);
            }
            if (!first) {
                out[size++] = ',';
            }
            strcpy(out + size, item_json);
            size += item_len;
            first = 0;
            free(item_json);
        }
    }
    
    if (size + 2 > cap) out = realloc(out, size + 2);
    out[size++] = ']';
    out[size] = '\0';

    mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "%s", out);
    
    free(out);
    free(resp);
}

void impl_get_product_by_id(void *ucbr, const char *id) {
    char log_buf[512];
    snprintf(log_buf, sizeof(log_buf), "impl_get_product_by_id: %s", id);
    log_info_c(log_buf);

    // Security check: validate ID to prevent shell injection
    for (const char* p = id; *p; p++) {
        if (!isalnum(*p) && *p != '-' && *p != '_') {
             mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Error: Invalid ID format.");
             return;
        }
    }

    char path[256];
    snprintf(path, sizeof(path), "/inventory/%s", id);
    char* resp = firestore_request("GET", path, NULL);
    
    if (resp) {
        log_info_c("Got response from Firestore");
    } else {
        log_info_c("Firestore request returned NULL");
    }

    if (!resp || strstr(resp, "\"error\"")) { 
         mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Product not found.");
         if (resp) free(resp);
         return;
    }
    
    char* item = doc_to_json(resp, strlen(resp));
    if (item) {
        mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "SUCCESS: %s", item);
        free(item);
    } else {
        log_info_c("doc_to_json returned NULL");
        mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "FAILED TO PARSE PRODUCT.");
    }
    free(resp);
}

// Hardcoded seed data (simplified)
const char* SEED_NAMES[] = {
    "Apples", "Bananas", "Milk", "Bread", "Eggs", "Cheese"
};

void impl_seed(void *ucbr) {
    // For each item, POST to /inventory
    int count = sizeof(SEED_NAMES)/sizeof(SEED_NAMES[0]);
    for (int i=0; i<count; i++) {
        char body[512];
        // Firestore format: fields: { ... }
        mjson_snprintf(body, sizeof(body), 
            "{ \"fields\": { \"name\": { \"stringValue\": \"%s\" }, \"price\": { \"integerValue\": \"%d\" }, \"quantity\": { \"integerValue\": \"%d\" } } }",
            SEED_NAMES[i], (i+1)*2, 100);
            
        char* resp = firestore_request("POST", "/inventory", body);
        if (resp) free(resp);
    }
    mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Database seeded (simplified).");
}

void impl_reset(void *ucbr) {
    // 1. Get all docs
    char* resp = firestore_request("GET", "/inventory", NULL);
    if (!resp) {
        mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Failed to list for reset.");
        return;
    }
    
    const char *docs;
    int docs_len;
    if (mjson_find(resp, strlen(resp), "$.documents", &docs, &docs_len) == MJSON_TOK_ARRAY) {
        int offset = 0;
        int k_off, k_len, v_off, v_len, v_type;
        while ((offset = mjson_next(docs, docs_len, offset, &k_off, &k_len, &v_off, &v_len, &v_type)) != 0) {
             // Extract name
             char name_path[256];
             if (mjson_get_string(docs + v_off, v_len, "$.name", name_path, sizeof(name_path)) > 0) {
                 // name_path is projects/.../databases/.../documents/inventory/ID
                 // We need to DELETE this URL.
                 // URL: https://firestore.googleapis.com/v1/{name_path}
                 
                 // extract relative path from documents
                 char* rel = strstr(name_path, "/documents/");
                 if (rel) {
                     rel += 10; // skip /documents
                     firestore_request("DELETE", rel, NULL);
                 }
             }
        }
    }
    
    free(resp);
    mcpc_toolcall_result_add_text_printf8((mcpc_ucbr_t*)ucbr, "Database reset.");
}

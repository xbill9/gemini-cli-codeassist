#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/utsname.h>
#include <time.h>
#include <curl/curl.h>

#include "mcpc/mcpc.h"
#include "mcpc/src/mjson.h"

#define PARAM_BUF_SIZE 4096
#define TIME_BUF_SIZE 30

static void print_json_escaped(FILE *out, const char *str) {
  if (!str) return;
  for (const char *p = str; *p; p++) {
    switch (*p) {
    case '"': fputc(92, out); fputc('"', out); break;
    case '\\': fputc(92, out); fputc(92, out); break;
    case '\b': fputc(92, out); fputc('b', out); break;
    case '\f': fputc(92, out); fputc('f', out); break;
    case '\n': fputc(92, out); fputc('n', out); break;
    case '\r': fputc(92, out); fputc('r', out); break;
    case '\t': fputc(92, out); fputc('t', out); break;
    default:
      if ((unsigned char)*p < 0x20) { fputc(92, out); fprintf(out, "u%04x", (unsigned char)*p); }
      else fputc(*p, out);
      break;
    }
  }
}

static void log_json(const char *level, const char *msg) {
  time_t now;
  time(&now);
  struct tm tm_info;
  gmtime_r(&now, &tm_info);
  char buf[TIME_BUF_SIZE];
  strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);
  
  fprintf(stderr, "{\"asctime\": \"%s\", ", buf); 
  fprintf(stderr, "\"name\": \"root\", \"levelname\": \"%s\", \"message\": \"", level);
  print_json_escaped(stderr, msg);
  fprintf(stderr, "\"}\n");
  fflush(stderr);
}

static void log_info(const char *msg) {
  log_json("INFO", msg);
}

static void log_error(const char *msg) {
  log_json("ERROR", msg);
}

static int exec_cmd_output(const char *cmd, char *buf, size_t size) {
  FILE *fp = popen(cmd, "r");
  if (!fp) return -1;
  if (fgets(buf, size, fp) == NULL) {
      pclose(fp);
      return -1;
  }
  pclose(fp);
  size_t len = strlen(buf);
  while (len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r')) {
    buf[--len] = '\0';
  }
  return 0;
}

static int get_project_id(char *buf, size_t size) {
  static char cached_project_id[128] = {0};
  if (cached_project_id[0] != '\0') {
    strncpy(buf, cached_project_id, size);
    return 0;
  }
  int ret = exec_cmd_output("gcloud config get-value project", buf, size);
  if (ret == 0) {
    strncpy(cached_project_id, buf, sizeof(cached_project_id));
  }
  return ret;
}

static int get_access_token(char *buf, size_t size) {
  static char cached_token[4096] = {0};
  static time_t last_fetch_time = 0;
  time_t now = time(NULL);

  // Cache token for 50 minutes (gcloud tokens usually last 1 hour)
  if (cached_token[0] != '\0' && (now - last_fetch_time) < 3000) {
    strncpy(buf, cached_token, size);
    return 0;
  }

  int ret = exec_cmd_output("gcloud auth print-access-token", buf, size);
  if (ret == 0) {
    strncpy(cached_token, buf, sizeof(cached_token));
    last_fetch_time = now;
  }
  return ret;
}

struct string_buf {
  char *ptr;
  size_t len;
};

static void init_string_buf(struct string_buf *s) {
  s->len = 0;
  s->ptr = malloc(s->len + 1);
  if (s->ptr == NULL) {
    log_error("malloc() failed");
    exit(EXIT_FAILURE);
  }
  s->ptr[0] = '\0';
}

static size_t write_func(void *ptr, size_t size, size_t nmemb, struct string_buf *s) {
  size_t new_len = s->len + size * nmemb;
  s->ptr = realloc(s->ptr, new_len + 1);
  if (s->ptr == NULL) {
    log_error("realloc() failed");
    exit(EXIT_FAILURE);
  }
  memcpy(s->ptr + s->len, ptr, size * nmemb);
  s->ptr[new_len] = '\0';
  s->len = new_len;
  return size * nmemb;
}

static int perform_firestore_request(const char *method, const char *path_suffix, 
                                     const char *json_body, char **response) {
  CURL *curl;
  CURLcode res;
  struct string_buf s;
  init_string_buf(&s);
  
  char project_id[128];
  if (get_project_id(project_id, sizeof(project_id)) != 0) {
      log_error("Failed to get project ID");
      free(s.ptr);
      return -1;
  }
  
  char token[4096];
  if (get_access_token(token, sizeof(token)) != 0) {
      log_error("Failed to get access token");
      free(s.ptr);
      return -1;
  }

  char url[2048];
  snprintf(url, sizeof(url), 
           "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents%s", 
           project_id, path_suffix);

  curl = curl_easy_init();
  if(curl) {
    struct curl_slist *headers = NULL;
    char auth_header[4200];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", token);
    headers = curl_slist_append(headers, auth_header);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_func);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &s);
    
    if (strcmp(method, "POST") == 0) {
        curl_easy_setopt(curl, CURLOPT_POST, 1L);
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_body ? json_body : "{}");
    } else if (strcmp(method, "PATCH") == 0) {
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "PATCH");
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_body ? json_body : "{}");
    } else if (strcmp(method, "DELETE") == 0) {
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
        if (json_body) curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_body);
    }

    res = curl_easy_perform(curl);
    
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);

    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);

    if (res != CURLE_OK) {
        log_error(curl_easy_strerror(res));
        free(s.ptr);
        return -1;
    }
    
    if (response_code >= 400) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Firestore API Error: %ld", response_code);
        log_error(err_msg);
        log_error(s.ptr);
        free(s.ptr);
        return -1;
    }

    *response = s.ptr;
    return 0;
  }
  free(s.ptr);
  return -1;
}

static void extract_id_from_name(const char *name_path, char *id_buf, size_t size) {
    const char *p = strrchr(name_path, '/');
    if (p) {
        snprintf(id_buf, size, "%s", p + 1);
    } else {
        snprintf(id_buf, size, "%s", name_path);
    }
}

static char *document_to_product_json(const char *doc_json, int len) {
    char name_path[512] = {0};
    char name[128] = {0};
    char imgfile[256] = {0};
    double price = 0;
    int quantity = 0;
    char timestamp[64] = {0};
    char actualdateadded[64] = {0};

    mjson_get_string(doc_json, len, "$.name", name_path, sizeof(name_path));
    if (name_path[0] == '\0') return NULL;

    mjson_get_string(doc_json, len, "$.fields.name.stringValue", name, sizeof(name));
    mjson_get_string(doc_json, len, "$.fields.imgfile.stringValue", imgfile, sizeof(imgfile));
    
    if (mjson_get_number(doc_json, len, "$.fields.price.doubleValue", &price) == 0) {
        char buf[32];
        if (mjson_get_string(doc_json, len, "$.fields.price.integerValue", buf, sizeof(buf)) > 0) {
            price = atof(buf);
        }
    }

    char q_buf[32];
    if (mjson_get_string(doc_json, len, "$.fields.quantity.integerValue", q_buf, sizeof(q_buf)) > 0) {
        quantity = atoi(q_buf);
    }
    
    mjson_get_string(doc_json, len, "$.fields.timestamp.timestampValue", timestamp, sizeof(timestamp));
    mjson_get_string(doc_json, len, "$.fields.actualdateadded.timestampValue", actualdateadded, sizeof(actualdateadded));

    char id[128];
    extract_id_from_name(name_path, id, sizeof(id));

    return mjson_aprintf("{%Q:%Q, %Q:%Q, %Q:%g, %Q:%d, %Q:%Q, %Q:%Q, %Q:%Q}",
                         "id", id,
                         "name", name,
                         "price", price,
                         "quantity", quantity,
                         "imgfile", imgfile,
                         "timestamp", timestamp,
                         "actualdateadded", actualdateadded);
}

static char *get_current_timestamp(void) {
    time_t now;
    time(&now);
    struct tm tm_info;
    gmtime_r(&now, &tm_info);
    static char buf[32];
    strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);
    return buf;
}

static void greet_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  char param[PARAM_BUF_SIZE];
  size_t len = 0;
  // Removed verbose log
  mcpc_errcode_t err = mcpc_tool_get_tpropval_u8str(tool, "param", param, sizeof(param), &len);
  if (err) {
    mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Error retrieving 'param'");
    return;
  }
  mcpc_toolcall_result_add_text_printf8(ucbr, "%s", param);
}

static void list_products_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    (void)tool;
    // Removed verbose log
    char *response = NULL;
    if (perform_firestore_request("GET", "/inventory", NULL, &response) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Failed to fetch products");
        return;
    }

    struct string_buf result;
    init_string_buf(&result);
    write_func("{\"products\":[", 13, 1, &result);

    const char *docs;
    int docs_len;
    if (mjson_find(response, strlen(response), "$.documents", &docs, &docs_len) == MJSON_TOK_ARRAY) {
        int k_off, k_len, v_off, v_len, v_type;
        int offset = 0;
        int count = 0;
        while ((offset = mjson_next(docs, docs_len, offset, &k_off, &k_len, &v_off, &v_len, &v_type)) != 0) {
            if (count > 0) {
                write_func(",", 1, 1, &result);
            }
            char *prod_json = document_to_product_json(docs + v_off, v_len);
            if (prod_json) {
                write_func(prod_json, strlen(prod_json), 1, &result);
                free(prod_json);
                count++;
            }
        }
    }

    write_func("]}", 2, 1, &result);
    mcpc_toolcall_result_add_text_printf8(ucbr, "%s", result.ptr);
    free(result.ptr);
    free(response);
}

static void get_product_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    char id[256];
    size_t l;
    if (mcpc_tool_get_tpropval_u8str(tool, "id", id, sizeof(id), &l) != 0) {
         mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Missing id");
         return;
    }
    
    CURL *curl = curl_easy_init();
    char *encoded_id = curl_easy_escape(curl, id, 0);

    char path[512];
    snprintf(path, sizeof(path), "/inventory/%s", encoded_id);

    curl_free(encoded_id);
    curl_easy_cleanup(curl);

    char *response = NULL;
    if (perform_firestore_request("GET", path, NULL, &response) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Product not found or error");
        return;
    }
    char *prod_json = document_to_product_json(response, strlen(response));
    if (prod_json) {
        mcpc_toolcall_result_add_text_printf8(ucbr, "%s", prod_json);
        free(prod_json);
    } else {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Error parsing product");
    }
    free(response);
}

static void add_product_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    char name[256], imgfile[256], temp[64], q_str[32];
    double price;
    int quantity;
    size_t l;
    mcpc_tool_get_tpropval_u8str(tool, "name", name, sizeof(name), &l);
    mcpc_tool_get_tpropval_u8str(tool, "imgfile", imgfile, sizeof(imgfile), &l);
    mcpc_tool_get_tpropval_u8str(tool, "price", temp, sizeof(temp), &l);
    price = atof(temp);
    mcpc_tool_get_tpropval_u8str(tool, "quantity", temp, sizeof(temp), &l);
    quantity = atoi(temp);
    // Removed verbose log
    char *ts = get_current_timestamp();
    snprintf(q_str, sizeof(q_str), "%d", quantity);
    char *body = mjson_aprintf(
        "{\"fields\":{\"name\":{\"stringValue\":%Q}, \"price\":{\"doubleValue\":%g}, \"quantity\":{\"integerValue\":%Q}, \"imgfile\":{\"stringValue\":%Q}, \"timestamp\":{\"timestampValue\":%Q}, \"actualdateadded\":{\"timestampValue\":%Q}}}",
        name,
        price,
        q_str,
        imgfile,
        ts,
        ts
    );
    char *response = NULL;
    if (perform_firestore_request("POST", "/inventory", body, &response) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Failed to add product");
    } else {
        mcpc_toolcall_result_add_text_printf8(ucbr, "Product added successfully");
        free(response);
    }
    free(body);
}

static void update_product_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    char id[256], name[256], imgfile[256], temp[64], q_str[32];
    double price;
    int quantity;
    size_t l;
    if (mcpc_tool_get_tpropval_u8str(tool, "id", id, sizeof(id), &l) != 0) {
         mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Missing id");
         return;
    }
    mcpc_tool_get_tpropval_u8str(tool, "name", name, sizeof(name), &l);
    mcpc_tool_get_tpropval_u8str(tool, "imgfile", imgfile, sizeof(imgfile), &l);
    mcpc_tool_get_tpropval_u8str(tool, "price", temp, sizeof(temp), &l);
    price = atof(temp);
    mcpc_tool_get_tpropval_u8str(tool, "quantity", temp, sizeof(temp), &l);
    quantity = atoi(temp);
    
    char *ts = get_current_timestamp();
    snprintf(q_str, sizeof(q_str), "%d", quantity);
    
    // Construct body WITHOUT actualdateadded
    char *body = mjson_aprintf(
        "{\"fields\":{\"name\":{\"stringValue\":%Q}, \"price\":{\"doubleValue\":%g}, \"quantity\":{\"integerValue\":%Q}, \"imgfile\":{\"stringValue\":%Q}, \"timestamp\":{\"timestampValue\":%Q}}}",
        name,
        price,
        q_str,
        imgfile,
        ts
    );

    CURL *curl = curl_easy_init();
    char *encoded_id = curl_easy_escape(curl, id, 0);
    
    char path[1024];
    // Use updateMask to preserve other fields (like actualdateadded)
    snprintf(path, sizeof(path), 
             "/inventory/%s?updateMask.fieldPaths=name&updateMask.fieldPaths=price&updateMask.fieldPaths=quantity&updateMask.fieldPaths=imgfile&updateMask.fieldPaths=timestamp", 
             encoded_id);
    
    curl_free(encoded_id);
    curl_easy_cleanup(curl);

    char *response = NULL;
    if (perform_firestore_request("PATCH", path, body, &response) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Failed to update product");
    } else {
        mcpc_toolcall_result_add_text_printf8(ucbr, "Product updated successfully");
        free(response);
    }
    free(body);
}

static void delete_product_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    char id[256];
    size_t l;
    if (mcpc_tool_get_tpropval_u8str(tool, "id", id, sizeof(id), &l) != 0) {
         mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Missing id");
         return;
    }
    
    CURL *curl = curl_easy_init();
    char *encoded_id = curl_easy_escape(curl, id, 0);
    
    char path[512];
    snprintf(path, sizeof(path), "/inventory/%s", encoded_id);
    
    curl_free(encoded_id);
    curl_easy_cleanup(curl);

    char *response = NULL;
    if (perform_firestore_request("DELETE", path, NULL, &response) != 0) {
         mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Failed to delete product");
    } else {
         mcpc_toolcall_result_add_text_printf8(ucbr, "Product deleted");
         free(response);
    }
}

static void find_products_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    char name[256];
    size_t l;
    if (mcpc_tool_get_tpropval_u8str(tool, "name", name, sizeof(name), &l) != 0) {
         mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Missing name");
         return;
    }
    // Removed verbose log
    char *query = mjson_aprintf(
        "{\"structuredQuery\":{\"from\":[{\"collectionId\":\"inventory\"}], \"where\":{\"fieldFilter\": {\"field\": {\"fieldPath\": \"name\"}, \"op\": \"EQUAL\", \"value\": {\"stringValue\": %Q}}}}}}",
        name
    );
    char *response = NULL;
    if (perform_firestore_request("POST", ":runQuery", query, &response) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Query failed");
        free(query);
        return;
    }
    free(query);
    struct string_buf result;
    init_string_buf(&result);
    write_func("{\"products\":[", 13, 1, &result);
    int count = 0, k_off, k_len, v_off, v_len, v_type, offset = 0;
    while ((offset = mjson_next(response, strlen(response), offset, &k_off, &k_len, &v_off, &v_len, &v_type)) != 0) {
        const char *item = response + v_off;
        const char *doc_ptr;
        int doc_len;
        if (mjson_find(item, v_len, "$.document", &doc_ptr, &doc_len) == MJSON_TOK_OBJECT) {
            char *prod_json = document_to_product_json(doc_ptr, doc_len);
            if (prod_json) {
                if (count > 0) write_func(",", 1, 1, &result);
                write_func(prod_json, strlen(prod_json), 1, &result);
                free(prod_json);
                count++;
            }
        }
    }
    write_func("]}", 2, 1, &result);
    mcpc_toolcall_result_add_text_printf8(ucbr, "%s", result.ptr);
    free(result.ptr);
    free(response);
}

static void batch_delete_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
    char ids_str[4096];
    size_t l;
    if (mcpc_tool_get_tpropval_u8str(tool, "ids", ids_str, sizeof(ids_str), &l) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Missing ids (comma separated)");
        return;
    }
    // Removed verbose log
    char project_id[128];
    get_project_id(project_id, sizeof(project_id));
    struct string_buf body;
    init_string_buf(&body);
    char *start = mjson_aprintf("{\"writes\":[", "");
    write_func(start, strlen(start), 1, &body);
    free(start);
    char *saveptr;
    char *token = strtok_r(ids_str, ",", &saveptr);
    int count = 0;
    while (token) {
        while(*token == ' ') token++;
        if (count > 0) write_func(",", 1, 1, &body);
        char path[512];
        snprintf(path, sizeof(path), "projects/%s/databases/(default)/documents/inventory/%s", project_id, token);
        char *real_op = mjson_aprintf("{\"delete\": \"%s\"}", path);
        write_func(real_op, strlen(real_op), 1, &body);
        free(real_op);
        count++;
        token = strtok_r(NULL, ",", &saveptr);
    }
    write_func("]}", 2, 1, &body);
    char *response = NULL;
    if (perform_firestore_request("POST", ":commit", body.ptr, &response) != 0) {
        mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Batch delete failed");
    } else {
        mcpc_toolcall_result_add_text_printf8(ucbr, "Batch delete successful");
        free(response);
    }
    free(body.ptr);
}

static void system_info_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  (void)tool;
  // Removed verbose log
  struct utsname buffer;
  if (uname(&buffer) != 0) {
    mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Error getting system info");
    return;
  }
  mcpc_toolcall_result_add_text_printf8(
      ucbr,
      "System Name: %s\nNode Name: %s\nRelease: %s\nVersion: %s\nMachine: %s\n",
      buffer.sysname, buffer.nodename, buffer.release, buffer.version,
      buffer.machine);
}

static void server_info_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  (void)tool;
  // Removed verbose log
  mcpc_toolcall_result_add_text_printf8(ucbr, "Server Name: firestore-stdio-c\nLanguage: C\n");
}

static void current_time_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  (void)tool;
  // Removed verbose log
  mcpc_toolcall_result_add_text_printf8(ucbr, "%s", get_current_timestamp());
}

static int setup_tools(mcpc_server_t *server) {
  mcpc_tool_t *greet_tool = mcpc_tool_new2("greet", "Get a greeting from a local stdio server.");
  if (!greet_tool) return -1;
  mcpc_tool_addfre_toolprop(greet_tool, mcpc_toolprop_new2("param", "Greeting parameter", MCPC_U8STR));
  mcpc_tool_set_call_cb(greet_tool, greet_cb);
  mcpc_server_add_tool(server, greet_tool);

  mcpc_tool_t *list_tool = mcpc_tool_new2("list_products", "List all products in the inventory");
  mcpc_tool_set_call_cb(list_tool, list_products_cb);
  mcpc_server_add_tool(server, list_tool);

  mcpc_tool_t *get_tool = mcpc_tool_new2("get_product", "Get a product by ID");
  mcpc_tool_addfre_toolprop(get_tool, mcpc_toolprop_new2("id", "Product ID", MCPC_U8STR));
  mcpc_tool_set_call_cb(get_tool, get_product_cb);
  mcpc_server_add_tool(server, get_tool);

  mcpc_tool_t *add_tool = mcpc_tool_new2("add_product", "Add a new product");
  mcpc_tool_addfre_toolprop(add_tool, mcpc_toolprop_new2("name", "Name", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(add_tool, mcpc_toolprop_new2("price", "Price", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(add_tool, mcpc_toolprop_new2("quantity", "Quantity", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(add_tool, mcpc_toolprop_new2("imgfile", "Image File", MCPC_U8STR));
  mcpc_tool_set_call_cb(add_tool, add_product_cb);
  mcpc_server_add_tool(server, add_tool);

  mcpc_tool_t *upd_tool = mcpc_tool_new2("update_product", "Update an existing product");
  mcpc_tool_addfre_toolprop(upd_tool, mcpc_toolprop_new2("id", "ID", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(upd_tool, mcpc_toolprop_new2("name", "Name", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(upd_tool, mcpc_toolprop_new2("price", "Price", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(upd_tool, mcpc_toolprop_new2("quantity", "Quantity", MCPC_U8STR));
  mcpc_tool_addfre_toolprop(upd_tool, mcpc_toolprop_new2("imgfile", "Image File", MCPC_U8STR));
  mcpc_tool_set_call_cb(upd_tool, update_product_cb);
  mcpc_server_add_tool(server, upd_tool);

  mcpc_tool_t *del_tool = mcpc_tool_new2("delete_product", "Delete a product by ID");
  mcpc_tool_addfre_toolprop(del_tool, mcpc_toolprop_new2("id", "ID", MCPC_U8STR));
  mcpc_tool_set_call_cb(del_tool, delete_product_cb);
  mcpc_server_add_tool(server, del_tool);

  mcpc_tool_t *find_tool = mcpc_tool_new2("find_products", "Find products by name");
  mcpc_tool_addfre_toolprop(find_tool, mcpc_toolprop_new2("name", "Name", MCPC_U8STR));
  mcpc_tool_set_call_cb(find_tool, find_products_cb);
  mcpc_server_add_tool(server, find_tool);

  mcpc_tool_t *batch_tool = mcpc_tool_new2("batch_delete", "Delete multiple products by ID");
  mcpc_tool_addfre_toolprop(batch_tool, mcpc_toolprop_new2("ids", "Comma separated IDs", MCPC_U8STR));
  mcpc_tool_set_call_cb(batch_tool, batch_delete_cb);
  mcpc_server_add_tool(server, batch_tool);

  mcpc_tool_t *sys_tool = mcpc_tool_new2("get_system_info", "Get detailed system information.");
  mcpc_tool_set_call_cb(sys_tool, system_info_cb);
  mcpc_server_add_tool(server, sys_tool);

  mcpc_tool_t *srv_tool = mcpc_tool_new2("get_server_info", "Get information about this MCP server.");
  mcpc_tool_set_call_cb(srv_tool, server_info_cb);
  mcpc_server_add_tool(server, srv_tool);

  mcpc_tool_t *time_tool = mcpc_tool_new2("get_current_time", "Get the current UTC time.");
  mcpc_tool_set_call_cb(time_tool, current_time_cb);
  mcpc_server_add_tool(server, time_tool);

  return 0;
}

int main(void) {
  setvbuf(stdout, NULL, _IONBF, 0);
  curl_global_init(CURL_GLOBAL_ALL);
  
  log_info("Starting MCP server");
  
  mcpc_server_t *server = mcpc_server_new_iostrm(stdin, stdout);
  if (!server) {
    log_error("Failed to create server");
    return EXIT_FAILURE;
  }
  mcpc_server_set_nament(server, "firestore-stdio-c");
  mcpc_server_capa_enable_tool(server);
  if (setup_tools(server) != 0) {
    mcpc_server_close(server);
    return EXIT_FAILURE;
  }
  
  log_info("Server entering stdio loop");
  mcpc_server_start(server);
  mcpc_server_close(server);
  curl_global_cleanup();
  return EXIT_SUCCESS;
}

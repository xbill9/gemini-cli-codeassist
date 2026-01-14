#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "mcpc/mcpc.h"

#define PARAM_BUF_SIZE 4096
#define TIME_BUF_SIZE 30

// JSON Logging Helper
static void print_json_escaped(FILE *out, const char *str) {
  if (!str)
    return;
  for (const char *p = str; *p; p++) {
    switch (*p) {
    case '\"':
      fprintf(out, "\\\"");
      break;
    case '\\':
      fprintf(out, "\\\\");
      break;
    case '\b':
      fprintf(out, "\\b");
      break;
    case '\f':
      fprintf(out, "\\f");
      break;
    case '\n':
      fprintf(out, "\\n");
      break;
    case '\r':
      fprintf(out, "\\r");
      break;
    case '\t':
      fprintf(out, "\\t");
      break;
    default:
      if ((unsigned char)*p < 0x20) {
        fprintf(out, "\\u%04x", (unsigned char)*p);
      } else {
        fputc(*p, out);
      }
      break;
    }
  }
}

static void log_info(const char *msg) {
  time_t now;
  time(&now);
  struct tm tm_info;

  // Use gmtime_r for thread safety
  gmtime_r(&now, &tm_info);

  char buf[TIME_BUF_SIZE];
  strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);

  // Structured JSON logging with escaping
  fprintf(stderr,
          "{\"asctime\": \"%s\", \"name\": \"root\", \"levelname\": \"INFO\", "
          "\"message\": \"",
          buf);
  print_json_escaped(stderr, msg);
  fprintf(stderr, "\"}\n");
  fflush(stderr);
}

// Tool Callback
static void greet_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  char param[PARAM_BUF_SIZE];
  size_t len = 0;

  // Log execution
  log_info("Executed greet tool");

  mcpc_errcode_t err =
      mcpc_tool_get_tpropval_u8str(tool, "param", param, sizeof(param), &len);
  if (err) {
    mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Error retrieving 'param'");
    return;
  }

  // Return param
  mcpc_toolcall_result_add_text_printf8(ucbr, "%s", param);
}

// Setup Tools
static int setup_tools(mcpc_server_t *server) {
  // Define "greet" tool
  mcpc_tool_t *greet_tool =
      mcpc_tool_new2("greet", "Get a greeting from a local stdio server.");
  if (!greet_tool) {
    fprintf(stderr, "Failed to create tool\n");
    return -1;
  }

  // Define "param" argument
  mcpc_toolprop_t *param_prop =
      mcpc_toolprop_new2("param", "Greeting parameter", MCPC_U8STR);
  if (!param_prop) {
    fprintf(stderr, "Failed to create tool property\n");
    // Note: We cannot free greet_tool here because mcpc_tool_free is not
    // exposed in the public API. This results in a minor leak on fatal error,
    // which is acceptable since the program will exit immediately.
    return -1;
  }

  // Add property to tool (takes ownership)
  mcpc_tool_addfre_toolprop(greet_tool, param_prop);

  // Set callback
  mcpc_tool_set_call_cb(greet_tool, greet_cb);

  // Add tool to server
  mcpc_server_add_tool(server, greet_tool);

  return 0;
}

int main(void) {
  // Ensure stdout is unbuffered for reliable communication over stdio
  setvbuf(stdout, NULL, _IONBF, 0);

  // Initialize Server
  mcpc_server_t *server = mcpc_server_new_iostrm(stdin, stdout);
  if (!server) {
    fprintf(stderr, "Failed to create server\n");
    return EXIT_FAILURE;
  }

  // Set Server Name
  mcpc_server_set_nament(server, "hello-world-server");

  // Enable Tool Capabilities
  mcpc_server_capa_enable_tool(server);

  // Setup Tools
  if (setup_tools(server) != 0) {
    mcpc_server_close(server);
    return EXIT_FAILURE;
  }

  // Start Server Loop
  mcpc_server_start(server);

  // Cleanup
  mcpc_server_close(server);

  return EXIT_SUCCESS;
}

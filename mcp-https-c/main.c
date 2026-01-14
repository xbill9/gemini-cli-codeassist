#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/utsname.h>
#include <time.h>

#include "mcpc/mcpc.h"

#define PARAM_BUF_SIZE 4096
#define TIME_BUF_SIZE 30

typedef enum {
    LOG_LEVEL_INFO,
    LOG_LEVEL_ERROR
} LogLevel;

static const char* level_to_string(LogLevel level) {
    switch (level) {
        case LOG_LEVEL_INFO: return "INFO";
        case LOG_LEVEL_ERROR: return "ERROR";
        default: return "UNKNOWN";
    }
}

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

static void log_message(LogLevel level, const char *file, int line, const char *func, const char *fmt, ...) {
  time_t now;
  time(&now);
  struct tm tm_info;
  gmtime_r(&now, &tm_info);
  char time_buf[TIME_BUF_SIZE];
  strftime(time_buf, sizeof(time_buf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);

  char msg_buf[2048];
  va_list args;
  va_start(args, fmt);
  vsnprintf(msg_buf, sizeof(msg_buf), fmt, args);
  va_end(args);

  fprintf(stderr, "{\"asctime\": \"%s\", \"name\": \"mcp-https-c\", \"levelname\": \"%s\", \"filename\": \"",
          time_buf, level_to_string(level));
  print_json_escaped(stderr, file);
  fprintf(stderr, "\", \"lineno\": %d, \"funcname\": \"", line);
  print_json_escaped(stderr, func);
  fprintf(stderr, "\", \"message\": \"");
  print_json_escaped(stderr, msg_buf);
  fprintf(stderr, "\"}\n");
  fflush(stderr);
}

#define LOG_INFO(...) log_message(LOG_LEVEL_INFO, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define LOG_ERROR(...) log_message(LOG_LEVEL_ERROR, __FILE__, __LINE__, __func__, __VA_ARGS__)

// Tool Callback
static void greet_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  char param[PARAM_BUF_SIZE];
  size_t len = 0;

  // Log execution
  LOG_INFO("Executed greet tool");

  mcpc_errcode_t err =
      mcpc_tool_get_tpropval_u8str(tool, "param", param, sizeof(param), &len);
  if (err) {
    mcpc_toolcall_result_add_errmsg_printf8(ucbr, "Error retrieving 'param'");
    return;
  }

  // Return param
  mcpc_toolcall_result_add_text_printf8(ucbr, "%s", param);
}

// Tool: get_system_info
static void system_info_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  (void)tool;
  LOG_INFO("Executed get_system_info tool");

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

#ifdef __STDC_VERSION__
  mcpc_toolcall_result_add_text_printf8(ucbr, "C Standard: %ld\n",
                                        __STDC_VERSION__);
#endif
}

// Tool: get_server_info
static void server_info_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  (void)tool;
  LOG_INFO("Executed get_server_info tool");
  mcpc_toolcall_result_add_text_printf8(
      ucbr, "Server Name: hello-https-c\nLanguage: C\n");
}

// Tool: get_current_time
static void current_time_cb(const mcpc_tool_t *tool, mcpc_ucbr_t *ucbr) {
  (void)tool;
  LOG_INFO("Executed get_current_time tool");

  time_t now;
  time(&now);
  struct tm tm_info;
  gmtime_r(&now, &tm_info);

  char buf[TIME_BUF_SIZE];
  strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);

  mcpc_toolcall_result_add_text_printf8(ucbr, "%s", buf);
}

// Setup Tools
static int setup_tools(mcpc_server_t *server) {
  // Define "greet" tool
  mcpc_tool_t *greet_tool =
      mcpc_tool_new2("greet", "Get a greeting from a local HTTP server.");
  if (!greet_tool) {
    LOG_ERROR("Failed to create tool");
    return -1;
  }

  // Define "param" argument
  mcpc_toolprop_t *param_prop =
      mcpc_toolprop_new2("param", "Greeting parameter", MCPC_U8STR);
  if (!param_prop) {
    LOG_ERROR("Failed to create tool property");
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

  // Define "get_system_info" tool
  mcpc_tool_t *sys_tool = mcpc_tool_new2(
      "get_system_info", "Get detailed system information (OS, Kernel, Arch).");
  if (!sys_tool)
    return -1;
  mcpc_tool_set_call_cb(sys_tool, system_info_cb);
  mcpc_server_add_tool(server, sys_tool);

  // Define "get_server_info" tool
  mcpc_tool_t *srv_tool = mcpc_tool_new2(
      "get_server_info", "Get information about this MCP server.");
  if (!srv_tool)
    return -1;
  mcpc_tool_set_call_cb(srv_tool, server_info_cb);
  mcpc_server_add_tool(server, srv_tool);

  // Define "get_current_time" tool
  mcpc_tool_t *time_tool =
      mcpc_tool_new2("get_current_time", "Get the current UTC time.");
  if (!time_tool)
    return -1;
  mcpc_tool_set_call_cb(time_tool, current_time_cb);
  mcpc_server_add_tool(server, time_tool);

  return 0;
}

int main(void) {
  LOG_ERROR("Attempting to create server...");

  // Initialize Server (TCP on port 8080)
  mcpc_server_t *server = mcpc_server_new_tcp();
  if (!server) {
    LOG_ERROR("Failed to create server");
    return EXIT_FAILURE;
  }

  LOG_ERROR("Setting server name...");

  // Set Server Name
  mcpc_server_set_nament(server, "hello-https-c");

  LOG_ERROR("Enabling Tools...");
  // Enable Tool Capabilities
  mcpc_server_capa_enable_tool(server);

  LOG_ERROR("Setting Up Tools...");
  // Setup Tools
  if (setup_tools(server) != 0) {
    mcpc_server_close(server);
    return EXIT_FAILURE;
  }
  LOG_ERROR("Server Starting...");

  // Start Server Loop
  mcpc_server_start(server);

  // Cleanup
  mcpc_server_close(server);

  return EXIT_SUCCESS;
}

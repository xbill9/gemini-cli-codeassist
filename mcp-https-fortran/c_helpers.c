#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <time.h>
#include "mcpc/mcpc.h"

/* Prototypes to satisfy -Wmissing-prototypes */
void helper_add_text_result(mcpc_ucbr_t *ucbr, const char *text);
void log_info_c(const char *msg);

void helper_add_text_result(mcpc_ucbr_t *ucbr, const char *text) {
    mcpc_toolcall_result_add_text_printf8(ucbr, "Hello, %s!", text);
}

static void print_json_escaped(FILE *out, const char *str) {
  if (!str)
    return;
  for (const char *p = str; *p; p++) {
    switch (*p) {
    case '"':
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

void log_info_c(const char *msg) {
  time_t now;
  time(&now);
  struct tm tm_info;

  // Use gmtime_r for thread safety
  gmtime_r(&now, &tm_info);

  char buf[30];
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
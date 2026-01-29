// This file is part of the mcpc library.
//
// Copyright (C) 2025 Michael Lee <micl2e2 AT proton.me>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions: 
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software. 
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#define _POSIX_C_SOURCE 200809L
#include "log.h"
#include <time.h>
#include <string.h>

LogLevel glv = Debug;

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

void
lvlog (LogLevel lv, const char *meta, FILE *stream, const char *format, ...)
{
  (void)stream; // Always use stderr for logging per requirement

  if (lv > glv)
    return;

  const char *levelname = "ERROR";
  if (lv == Info)
    levelname = "INFO";
  else if (lv == Debug)
    levelname = "DEBUG";

  time_t now;
  time (&now);
  struct tm tm_info;
#if defined(is_unix)
  gmtime_r (&now, &tm_info);
#elif defined(is_win)
  gmtime_s (&tm_info, &now);
#else
  struct tm *tmp = gmtime (&now);
  if (tmp) tm_info = *tmp;
#endif

  char tbuf[30];
  strftime (tbuf, sizeof (tbuf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);

  char msg_buf[2048];
  va_list args;
  va_start (args, format);
  vsnprintf (msg_buf, sizeof (msg_buf), format, args);
  va_end (args);

  fprintf (stderr,
           "{\"asctime\": \"%s\", \"name\": \"mcpc\", \"levelname\": \"%s\", "
           "\"meta\": \"%s\", \"message\": \"",
           tbuf, levelname, meta ? meta : "");
  print_json_escaped (stderr, msg_buf);
  fprintf (stderr, "\"}\n");
  fflush (stderr);
}

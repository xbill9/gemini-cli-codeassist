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
#include "errcode.h"

#include <stdlib.h>
#include <stdio.h>

mcpc_errcode_t mcpc_ec = MCPC_EC_0;

#include <time.h>
void
mcpc_assert (bool condition, mcpc_errcode_t ec)
{
  if (!condition)
    {
      time_t now;
      time(&now);
      struct tm tm_info;
#if defined(is_unix)
      gmtime_r(&now, &tm_info);
#elif defined(is_win)
      gmtime_s(&tm_info, &now);
#else
      struct tm *tmp = gmtime(&now);
      if (tmp) tm_info = *tmp;
#endif
      char tbuf[30];
      strftime(tbuf, sizeof(tbuf), "%Y-%m-%dT%H:%M:%SZ", &tm_info);

      fprintf(stderr, "{\"asctime\": \"%s\", \"name\": \"mcpc\", \"levelname\": \"CRITICAL\", \"message\": \"ASSERTION FAILED: ec=%d\"}\n", tbuf, ec);
      fflush(stderr);
      abort ();
    }
}

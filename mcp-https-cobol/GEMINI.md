# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **COBOL-based Model Context Protocol (MCP) server** named `mcp-https-cobol`, implemented using the `mcpc` library via C bindings. It exposes a single tool (`greet`) over **HTTP** and logs structured JSON to `stderr`.

## Key Technologies

*   **Language:** COBOL (GnuCOBOL `cobc`), C (C17 Helper functions)
*   **Library:** `mcpc` (embedded in `mcpc/` directory)
*   **Build System:** `make`
*   **Compiler:** `cobc` (GnuCOBOL), `cc` (C)
*   **Communication:** HTTP (TCP) for MCP; structured JSON via `stderr` for logging.
*   **Testing:** Python 3 (`test_server_http.py`)

## Project Structure

* `server.cob`: The main entry point and tool implementation in COBOL.
* `c_helpers.c`: C helper functions for structured JSON logging and bridging macros.
* `cob_helpers.c`: C wrapper functions to handle argument passing safely from `mcpc` callbacks into COBOL programs.
* `server.c`, `server.c.h`, `server.c.l1.h`, `server.c.l2.h`, `server.i`: C source code, headers, and intermediate files generated from `server.cob` by GnuCOBOL.
* `Makefile`: Build configuration for all components.
* `test_server_http.py`: Python script for integration testing the compiled COBOL server.
* `mcpc/`: The `mcpc` library source code.



## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local server.
*   **Parameters:**
    *   `param` (string): Greeting parameter.
*   **Implementation:** `greet_cb_impl` program in `server.cob`, invoked via `greet_cb_wrapper` in `cob_helpers.c`.
*   **Output:** Returns "Hello, [param]!".

## API Usage (COBOL Interface)

The project uses `CALL` statements to invoke C functions from `mcpc` and helper files.

## Implementation Notes (`mcpc` v0.1)

*   **Buffer Management:** The `mcpc` library (v0.1) requires manual buffer management (`ensure_buf_suffi`) for the server's response buffer (`sv->rpcres`) before calling `jsonrpc_return_success2`, as it does not automatically resize for the JSON-RPC wrapper and newline.
*   **64-bit Compatibility:** There is a known issue on 64-bit systems where `size_t` lengths passed to `mjson` printf format specifiers (like `%.*s`, `%.*Q`) cause segmentation faults; casting to `(int)` resolves this.

### Server Lifecycle
*   **Initialization:** `mcpc_server_new_tcp`.
*   **Naming:** `mcpc_server_set_nament`.
*   **Capabilities:** `mcpc_server_capa_enable_tool`.
*   **Execution:** `mcpc_server_start` and `mcpc_server_close`.

### Tool Definition
*   **Tool Creation:** `mcpc_tool_new2`.
*   **Property Creation:** `mcpc_toolprop_new2` (type `9` for `MCPC_U8STR` / string).
*   **Adding Properties:** `mcpc_tool_addfre_toolprop`.
*   **Callback Registration:** `mcpc_tool_set_call_cb`.

### Helper Functions & Logging
*   `log_info_c`: Emits structured JSON logs to `stderr`.
*   `helper_add_text_result`: Formats a response as "Hello, %s!".
*   `helper_add_text_raw`: Adds raw text to the tool result.

## Build & Development

The `Makefile` supports the following targets:

*   `make server-cobol`: Builds the COBOL server binary.
*   `make debug`: Builds with debug symbols and no optimizations.
*   `make release`: Builds with optimizations and stripped binaries.
*   `make test`: Runs integration tests for the COBOL server.
*   `make lint`: Lints C helper code.
*   `make check`: Runs linting and all tests.
*   `make format`: Formats C code using `clang-format`.
*   `make clean`: Removes build artifacts.

## Running the Server

```bash
./server-cobol
```

*   **Input:** JSON-RPC 2.0 messages via HTTP POST (port 8080 default).
*   **Output:** JSON-RPC 2.0 messages via HTTP response.
*   **Logs:** JSON formatted logs via `stderr`.

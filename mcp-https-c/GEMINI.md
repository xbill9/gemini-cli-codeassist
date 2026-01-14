# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **C-based Model Context Protocol (MCP) server** named `hello-https-c`, implemented using the `mcpc` library. It exposes a single tool (`greet`) over HTTP and logs structured JSON to stderr.

Do not use stdio transport for this project

## Key Technologies

*   **Language:** C (C17 Standard)
*   **Library:** `mcpc` (embedded in `mcpc/` directory)
*   **Build System:** `make`
*   **Communication:** HTTP/1.1 over TCP (Port 8080) for MCP; `stderr` for logging.
*   **Testing:** Python 3 (`test_server.py`)

## Project Structure

*   `main.c`: The entry point. Initializes the `mcpc` server, defines the `greet` tool, and handles the event loop.
*   `Makefile`: Build configuration with targets for building, linting, testing, and formatting.
*   `test_server.py`: Python script for integration testing the compiled server.
*   `mcpc/`: The `mcpc` library source code (submodule).

## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local HTTP server.
*   **Parameters:**
    *   `param` (string): Greeting parameter.
*   **Implementation:** `greet_cb` function in `main.c`.

### `get_system_info`
*   **Description:** Get detailed system information (OS, Kernel, Arch).
*   **Implementation:** `system_info_cb` function in `main.c`.

### `get_server_info`
*   **Description:** Get information about this MCP server.
*   **Implementation:** `server_info_cb` function in `main.c`.

### `get_current_time`
*   **Description:** Get the current UTC time.
*   **Implementation:** `current_time_cb` function in `main.c`.

### `mcpc-info`
*   **Description:** Built-in tool that returns information about the `mcpc` library.

## API Usage (mcpc)

The project uses the `mcpc` library. Key functions used in `main.c`:

*   **Server Initialization:** `mcpc_server_new_tcp()`
*   **Server Naming:** `mcpc_server_set_nament`
*   **Capabilities:** `mcpc_server_capa_enable_tool`
*   **Tool Creation:** `mcpc_tool_new2`
*   **Property Creation:** `mcpc_toolprop_new2` (type `MCPC_U8STR`)
*   **Adding Properties:** `mcpc_tool_addfre_toolprop`
*   **Callback Registration:** `mcpc_tool_set_call_cb`
*   **Server Loop:** `mcpc_server_start`
*   **Argument Parsing:** `mcpc_tool_get_tpropval_u8str`
*   **Result Construction:** `mcpc_toolcall_result_add_text_printf8`

## Build & Development

The `Makefile` supports the following targets:

*   `make` (or `make all`): Builds the `server` binary and `libmcpc.a`.
*   `make lint`: Runs static analysis using stricter compiler flags (`-fsyntax-only`).
*   `make test`: Runs the integration tests using `python3 test_server.py`.
*   `make check`: Runs both `lint` and `test`.
*   `make format`: Formats `main.c` using `clang-format`.
*   `make clean`: Removes build artifacts.

## Running the Server

```bash
./server
```

*   **Input:** HTTP POST requests containing JSON-RPC 2.0 messages via TCP (Port 8080).
*   **Output:** HTTP/1.1 200 OK responses containing JSON-RPC 2.0 messages.
*   **Logs:** JSON formatted logs via `stderr`.

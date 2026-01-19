# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **C-based Model Context Protocol (MCP) server** named `firestore-stdio-c`, implemented using the `mcpc` library. It exposes a single tool (`greet`) over standard input/output (stdio) and logs structured JSON to stderr.

## Key Technologies

*   **Language:** C (C17 Standard)
*   **Library:** `mcpc` (embedded in `mcpc/` directory)
*   **Networking:** `libcurl` for Firestore API requests.
*   **JSON Handling:** `mjson` (embedded in `mcpc/src/mjson.h`).
*   **Build System:** `make`
*   **Communication:** Standard Input/Output (stdio) for MCP; `stderr` for logging.
*   **Testing:** Python 3 (`test_server.py`)

## Project Structure

*   `main.c`: The entry point. Initializes the `mcpc` server, defines all tools (Firestore inventory and system info), and handles the event loop.
*   `Makefile`: Build configuration with targets for building, linting, testing, and formatting.
*   `test_server.py`: Python script for integration testing the compiled server.
*   `mcpc/`: The `mcpc` library source code (submodule).

## Implemented Tools

### General Tools
*   `greet`: Get a greeting from the server.
*   `get_system_info`: Get OS and machine details.
*   `get_server_info`: Get info about this server instance.
*   `get_current_time`: Get current UTC time.

### Firestore Inventory Tools
*   `list_products`: List all documents in the `inventory` collection.
*   `get_product`: Fetch a specific product by ID.
*   `add_product`: Create a new product document.
*   `update_product`: Update an existing product document.
*   `delete_product`: Remove a product by ID.
*   `find_products`: Query products by name.
*   `batch_delete`: Commit multiple deletes in one request.
*   `inventory_report`: Generate a formatted text report of the inventory.
*   `generate_menu`: Generate a menu-style list of products and prices.

## API Usage (mcpc)

The project uses the `mcpc` library. Key functions used in `main.c`:

*   **Server Initialization:** `mcpc_server_new_iostrm(stdin, stdout)`
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

*   **Input:** JSON-RPC 2.0 messages via `stdin`.
*   **Output:** JSON-RPC 2.0 messages via `stdout`.
*   **Logs:** JSON formatted logs via `stderr`.
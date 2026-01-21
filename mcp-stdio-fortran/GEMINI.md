# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Fortran-based Model Context Protocol (MCP) server** named `mcp-stdio-fortran`, implemented using the `mcpc` library via C bindings (`iso_c_binding`). It exposes a single tool (`greet`) over standard input/output (stdio) and logs structured JSON to stderr.

## Key Technologies

*   **Language:** Fortran (2008+ with `iso_c_binding`), C (C17 Helper functions)
*   **Library:** `mcpc` (embedded in `mcpc/` directory)
*   **Build System:** `make`
*   **Compiler:** `gfortran` (Fortran), `cc` (C)
*   **Communication:** Standard Input/Output (stdio) for MCP; `stderr` for logging.
*   **Testing:** Python 3 (`test_server_fortran.py`)

## Project Structure

*   `server.f90`: The main entry point and tool implementation in Fortran.
*   `c_helpers.c`: C helper functions for `stdio` handling and bridging macros not easily accessible in Fortran.
*   `Makefile`: Build configuration.
*   `test_server_fortran.py`: Python script for integration testing the compiled Fortran server.
*   `mcpc/`: The `mcpc` library source code (submodule).

## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local stdio server.
*   **Parameters:**
    *   `param` (string): Greeting parameter.
*   **Implementation:** `greet_cb` subroutine in `server.f90`.

## API Usage (Fortran Interface)

The project defines an interface module `mcpc_interface` in `server.f90` to bind to `mcpc` C functions.

*   **Server Initialization:** `mcpc_server_new_iostrm` (via `get_stdin`/`get_stdout` C helpers)
*   **Server Naming:** `mcpc_server_set_nament`
*   **Capabilities:** `mcpc_server_capa_enable_tool`
*   **Tool Creation:** `mcpc_tool_new2`
*   **Property Creation:** `mcpc_toolprop_new2`
*   **Adding Properties:** `mcpc_tool_addfre_toolprop`
*   **Callback Registration:** `mcpc_tool_set_call_cb`
*   **Server Loop:** `mcpc_server_start`
*   **Argument Parsing:** `mcpc_tool_get_tpropval_u8str`
*   **Result Construction:** `helper_add_text_result` (C helper wrapping `mcpc_toolcall_result_add_text_printf8`)

## Build & Development

The `Makefile` supports the following targets:

*   `make server-fortran`: Builds the Fortran server binary.
*   `make test-fortran`: Runs integration tests for Fortran server.
*   `make clean`: Removes build artifacts.

## Running the Server

```bash
./server-fortran
```

*   **Input:** JSON-RPC 2.0 messages via `stdin`.
*   **Output:** JSON-RPC 2.0 messages via `stdout`.
*   **Logs:** JSON formatted logs via `stderr`.

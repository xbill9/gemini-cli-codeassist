# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **C++ based Model Context Protocol (MCP) server** named `mcp-https-cplus`, implemented using the `cpp-mcp` library. It exposes a single tool (`greet`) over HTTP/SSE (listening on `0.0.0.0:8080`) and logs structured JSON to stderr.

## Key Technologies

*   **Language:** C++ (C++17 Standard)
*   **Library:** `cpp-mcp` (embedded in `cpp-mcp/` directory)
*   **Build System:** `make`
*   **Communication:** HTTP/SSE (port 8080) for MCP; `stderr` for logging.
*   **Testing:** Python 3 (`test_server.py`)

Do Not use stdio transport for this project

## Project Structure

*   `main.cpp`: The entry point. Uses `mcp::server` to handle the MCP protocol over HTTP/SSE. It supports:
    *   `initialize`: Sets up the connection.
    *   `notifications/initialized`: Transitions the server to initialized state.
    *   `ping`: Standard connectivity check.
    *   `tools/list`: Lists registered tools.
    *   `tools/call`: Executes a tool.
*   `Makefile`: Build configuration with targets for building, testing, and cleaning.
*   `test_server.py`: Python script for integration testing the compiled server.
*   `cpp-mcp/`: The C++ MCP library source code (submodule).

## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local HTTP server.
*   **Parameters:**
    *   `param` (string): Greeting parameter.
*   **Implementation:** Returns the input `param` as a text content block.

## API Usage (cpp-mcp)

The project uses the `cpp-mcp` library. Key components used in `main.cpp`:

*   **mcp::tool**: Represents an MCP tool definition.
*   **mcp::tool_builder**: Fluent API for creating tools.
*   **mcp::request / mcp::response**: Helpers for JSON-RPC messages.
*   **nlohmann::json**: (Aliased as `mcp::json`) Used for JSON manipulation.

## Build & Development

The `Makefile` supports the following targets:

*   `make` (or `make all`): Builds the `server` binary with optimizations.
*   `make debug`: Builds with debug symbols (`-g -O0`).
*   `make release`: Builds with high optimization and strips the binary.
*   `make test`: Runs the integration tests using `python3 test_server.py`.
*   `make check`: Runs both `lint` and `test`.
*   `make lint`: Checks formatting with `clang-format`.
*   `make format`: Applies formatting with `clang-format`.
*   `make clean`: Removes build artifacts.

## Running the Server

```bash
./server
```

*   **Input:** JSON-RPC 2.0 messages via `stdin`.
*   **Output:** JSON-RPC 2.0 messages via `stdout`.
*   **Logs:** JSON formatted logs via `stderr`.

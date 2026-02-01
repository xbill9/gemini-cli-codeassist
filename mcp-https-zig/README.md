# MCP HTTPS Zig Server

A simple Model Context Protocol (MCP) server implemented in Zig. This server communicates over **HTTP** (defaulting to port 8080) and serves as a foundational "Hello World" example for Zig-based MCP integrations, utilizing the `mcp.zig` library.

## Overview

This project provides a basic MCP server named `mcp-https-zig` that exposes a single tool: `greet`.
- **Transport:** HTTP (custom implementation, listening on 0.0.0.0:8080).
- **Logging:** Structured JSON logs are written to `stderr`.

## Prerequisites

-   **Zig Compiler** (0.15.2 or later)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-zig
    ```

2.  **Build the server:**
    ```bash
    zig build -Doptimize=ReleaseSafe
    ```
    Or using `make`:
    ```bash
    make release
    ```

## Usage

To run the server:
```bash
./zig-out/bin/mcp-https-zig
```
Or via the convenience symlink created by `make`:
```bash
./server
```

The server will start listening on `0.0.0.0:8080`.

### Interacting with the Server

This server implements a basic HTTP transport where MCP JSON-RPC messages are sent as the body of HTTP POST requests.

**Example using `curl`:**

```bash
curl -X POST http://localhost:8080 \
     -H "Content-Type: application/json" \
     -d '{
       "jsonrpc": "2.0",
       "id": 1,
       "method": "tools/call",
       "params": {
         "name": "greet",
         "arguments": {
           "param": "Zig Developer"
         }
       }
     }'
```

## Tools

### `greet`
- **Description:** Get a greeting from a local http server.
- **Parameters:**
    - `param` (string): The text or name to echo back (defaults to "Stranger").
- **Returns:** The string passed in `param` as a greeting.

## Development

The project uses the Zig build system and a Makefile for convenience.

- **Build:** `zig build` or `make build`
- **Release:** `zig build -Doptimize=ReleaseSafe` or `make release`
- **Run:** `zig build run`
- **Test:** `zig build test` (Unit tests) and `python3 test_server.py` (Integration) or simply `make test`
- **Clean:** `rm -rf zig-out zig-cache` or `make clean`
- **Format:** `make format`
- **Lint:** `make lint`

## Project Structure

- `src/main.zig`: Entry point and HTTP transport implementation.
- `build.zig`: Zig build configuration.
- `build.zig.zon`: Zig package manager configuration (defines dependencies).
- `Makefile`: Convenience commands for building and testing.

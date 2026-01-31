# MCP Stdio Zig Server

A simple Model Context Protocol (MCP) server implemented in Zig. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for Zig-based MCP integrations, utilizing the `mcp.zig` library.

## Overview

This project provides a basic MCP server named `mcp-stdio-zig` that exposes a single tool: `greet`. It writes structured JSON logs to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **Zig Compiler** (0.15.2 or later)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-zig
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

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
./zig-out/bin/mcp-stdio-zig
```
Or via the convenience symlink created by `make`:
```bash
./server
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json` or VSCode settings), the configuration would look something like this:

```json
{
  "mcpServers": {
    "zig-hello-world": {
      "command": "/absolute/path/to/mcp-stdio-zig/zig-out/bin/mcp-stdio-zig",
      "args": []
    }
  }
}
```

*Note: Ensure the absolute path is correct.*

## Tools

### `greet`
- **Description:** Get a greeting from a local stdio server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
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

- `src/main.zig`: Entry point and Zig implementation.
- `build.zig`: Zig build configuration.
- `build.zig.zon`: Zig package manager configuration (defines dependencies).
- `Makefile`: Convenience commands for building and testing.
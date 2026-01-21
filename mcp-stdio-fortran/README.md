# MCP Stdio Fortran Server

A Model Context Protocol (MCP) server implemented in **Fortran** using the `mcpc` library via C bindings. This server communicates over `stdio` and demonstrates how to build MCP servers in Fortran.

## Overview

This project provides an MCP server named `mcp-stdio-fortran` (binary: `server-fortran`) that exposes a single tool: `greet`. It writes structured JSON logs to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **Fortran Compiler** (e.g., `gfortran`)
-   **C Compiler** (GCC, Clang, etc.) supporting C17
-   **Make**
-   **Python 3** (for testing)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-fortran
    # Initialize the mcpc submodule
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make server-fortran
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
./server-fortran
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json` or VSCode settings), the configuration would look something like this:

```json
{
  "mcpServers": {
    "mcp-stdio-fortran": {
      "command": "/absolute/path/to/mcp-stdio-fortran/server-fortran",
      "args": []
    }
  }
}
```

*Note: Ensure the absolute path is correct.*

## Tools

### `greet`
- **Description:** Get a greeting from the local stdio server.
- **Parameters:**
    - `param` (string): Greeting parameter.
- **Returns:** A formatted greeting: "Hello, [param]!".

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build Fortran server:** `make server-fortran`
- **Lint C code:** `make lint`
- **Run Fortran tests:** `make test-fortran` (requires Python 3)
- **Format C code:** `make format`
- **Check all:** `make check`
- **Clean artifacts:** `make clean`

## Project Structure

- `server.f90`: The main Fortran implementation of the server and `greet` tool.
- `c_helpers.c`: C helper functions to bridge Fortran and the `mcpc` library.
- `Makefile`: Commands for building C and Fortran binaries.
- `test_server_fortran.py`: Integration tests for the Fortran server.
- `mcpc/`: The C MCP library submodule.

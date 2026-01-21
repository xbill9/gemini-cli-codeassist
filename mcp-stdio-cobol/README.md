# MCP Stdio COBOL Server

A Model Context Protocol (MCP) server implemented in **COBOL** (GnuCOBOL) using the `mcpc` library via C bindings. This server communicates over `stdio` and demonstrates how to build robust MCP servers in COBOL.

## Overview

This project provides an MCP server named `mcp-stdio-cobol` (binary: `server-cobol`) that exposes a single tool: `greet`. It writes structured JSON logs to `stderr`, ensuring that the `stdout` stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **GnuCOBOL Compiler** (`cobc`)
-   **C Compiler** (GCC, Clang, etc.) supporting C17
-   **Make**
-   **clang-format** (for `make format`)
-   **Python 3** (for integration testing)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-cobol
    ```

2.  **Build the server:**
    ```bash
    make server-cobol
    ```
    *(Note: This will automatically clone the `mcpc` library if it's missing.)*

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the `stdio` communication.

To run the server manually (starts listening for JSON-RPC on `stdin`):
```bash
./server-cobol
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look like this:

```json
{
  "mcpServers": {
    "mcp-stdio-cobol": {
      "command": "/absolute/path/to/mcp-stdio-cobol/server-cobol",
      "args": []
    }
  }
}
```

*Note: Always use absolute paths in MCP configurations.*

## Tools

### `greet`
- **Description:** Get a greeting from the local stdio server.
- **Parameters:**
    - `param` (string): Greeting parameter (e.g., your name).
- **Returns:** A formatted greeting: "Hello, [param]!".

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build COBOL server:** `make server-cobol`
- **Build with Debugging:** `make debug`
- **Build for Release:** `make release`
- **Run COBOL tests:** `make test`
- **Full Quality Check:** `make check` (Lint + Tests)
- **Lint C code:** `make lint`
- **Format C code:** `make format`
- **Clean artifacts:** `make clean`

## Project Structure

- `server.cob`: The main COBOL implementation of the server and tool logic.
- `c_helpers.c`: C helper functions for `stdio` management and structured JSON logging.
- `cob_helpers.c`: C wrapper to safely bridge MCP callbacks into COBOL.
- `Makefile`: Build configuration for all components.
- `test_server_cobol.py`: Integration tests for the COBOL server.
- `mcpc/`: The `mcpc` library source code.

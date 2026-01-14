# MCP Stdio C++ Server

A simple Model Context Protocol (MCP) server implemented in C++ using the `cpp-mcp` library. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for C++ based MCP integrations.

## Overview

This project provides a basic MCP server named `mcp-stdio-cplus` that exposes a single tool: `greet`. It writes structured JSON logs to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **C++ Compiler** (GCC, Clang, etc.) supporting C++17.
-   **Make**
-   **Python 3** (for running integration tests)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-cplus
    # Initialize the submodules
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
./server
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json` or VSCode settings), the configuration would look something like this:

```json
{
  "mcpServers": {
    "mcp-stdio-cplus": {
      "command": "/absolute/path/to/mcp-stdio-cplus/server",
      "args": []
    }
  }
}
```

*Note: Ensure the absolute path is correct.*

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param` as a greeting.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build:** `make` (Default release-like build)
- **Debug Build:** `make debug` (Includes debug symbols, no optimization)
- **Release Build:** `make release` (Full optimization, stripped binary)
- **Run tests:** `make test` (Requires Python 3)
- **Check:** `make check` (Runs lint and test)
- **Lint:** `make lint` (Checks code formatting using `clang-format`)
- **Format:** `make format` (Formats code using `clang-format`)
- **Clean:** `make clean` (Removes build artifacts)

*Note: `make lint` and `make format` require `clang-format` to be installed.*

## Project Structure

- `main.cpp`: Entry point using `cpp-mcp` to define the server and tools.
- `Makefile`: Commands for building the C++ binary.
- `cpp-mcp/`: The C++ MCP library submodule.
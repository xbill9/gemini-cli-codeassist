# MCP HTTPS C Server

A simple Model Context Protocol (MCP) server implemented in C using the `mcpc` library. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for C-based MCP integrations.

## Overview

This project provides a Firestore MCP server demo named `firestore-https-c` that allows managing a product inventory in Google Cloud Firestore. It supports tools for listing, getting, adding, updating, and deleting products. It writes structured JSON logs to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

### Tools
- `greet`: Basic greeting tool.
- `list_products`: List all products in the inventory.
- `get_product`: Get a product by ID.
- `add_product`: Add a new product.
- `update_product`: Update an existing product.
- `delete_product`: Delete a product by ID.
- `find_products`: Find products by name.
- `batch_delete`: Delete multiple products by ID.
- `get_system_info`: Get system information.
- `get_server_info`: Get server information.
- `get_current_time`: Get current UTC time.

## Prerequisites

-   **C Compiler** (GCC, Clang, etc.) supporting C17.
-   **Make**

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-c
    # Initialize the mcpc submodule
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
    "c-hello-world": {
      "command": "/absolute/path/to/mcp-https-c/server",
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

- **Build:** `make`
- **Lint code:** `make lint` (uses compiler static analysis)
- **Run tests:** `make test` (requires Python 3)
- **Format code:** `make format` (requires clang-format)
- **Check all:** `make check` (runs lint and test)
- **Clean artifacts:** `make clean`

## Project Structure

- `main.c`: Entry point using `mcpc` to define the server and tools.
- `Makefile`: Commands for building the C binary.
- `mcpc/`: The C MCP library submodule.
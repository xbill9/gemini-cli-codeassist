# MCP Stdio C Server

A simple Model Context Protocol (MCP) server implemented in C using the `mcpc` library. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for C-based MCP integrations.

## Overview

This project provides a basic MCP server named `firestore-stdio-c` that exposes a single tool: `greet`. It writes structured JSON logs to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **C Compiler** (GCC, Clang, etc.) supporting C17.
-   **Make**
-   **libcurl** (development headers)
-   **gcloud CLI** (configured and authenticated for Firestore access)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-c
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
    "firestore-stdio-c": {
      "command": "/absolute/path/to/firestore-stdio-c/server",
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

### Firestore Inventory Tools
- `list_products`: List all products in the inventory.
- `get_product`: Get a product by ID (parameter: `id`).
- `add_product`: Add a new product (parameters: `name`, `price`, `quantity`, `imgfile`).
- `update_product`: Update an existing product (parameters: `id`, `name`, `price`, `quantity`, `imgfile`).
- `delete_product`: Delete a product by ID (parameter: `id`).
- `find_products`: Find products by name (parameter: `name`).
- `batch_delete`: Delete multiple products by ID (parameter: `ids`, comma separated).
- `inventory_report`: Prints a detailed inventory report.
- `generate_menu`: Generates a menu from the inventory.

### System Tools
- `get_system_info`: Get detailed system information (OS, version, etc.).
- `get_server_info`: Get information about this MCP server.
- `get_current_time`: Get the current UTC time.

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
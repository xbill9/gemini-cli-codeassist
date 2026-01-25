# Firestore Stdio C++ Server

A simple Model Context Protocol (MCP) server implemented in C++ using the `cpp-mcp` library. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for C++ based MCP integrations.

## Overview

This project provides a basic MCP server named `firestore-stdio-cplus` that exposes a single tool: `greet`. It writes structured JSON logs to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **C++ Compiler** (GCC, Clang, etc.) supporting C++17.
-   **Make**
-   **Python 3** (for running integration tests)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-cplus
    # Initialize the submodules
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

### Environment Configuration

To successfully connect to Firestore, you must configure the following environment variables:

1.  **Project ID (Required):**
    *   `GOOGLE_CLOUD_PROJECT` OR `FIREBASE_PROJECT_ID`: Set this to your Google Cloud or Firebase project ID.
2.  **Authentication (Optional but recommended):**
    *   `FIRESTORE_ACCESS_TOKEN`: A valid OAuth 2.0 access token.
    *   *Fallback:* If `FIRESTORE_ACCESS_TOKEN` is not set, the server attempts to run `gcloud auth print-access-token` to fetch credentials. Ensure the `gcloud` CLI is installed and authenticated (`gcloud auth login`) in the environment where the server runs.

To run the server manually (starts listening on stdio):
```bash
export GOOGLE_CLOUD_PROJECT=my-project-id
./server
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json` or VSCode settings), the configuration would look something like this:

```json
{
  "mcpServers": {
    "firestore-stdio-cplus": {
      "command": "/absolute/path/to/firestore-stdio-cplus/server",
      "args": []
    }
  }
}
```

*Note: Ensure the absolute path is correct.*

## Tools

The server implements several tools for interacting with a Firestore-backed inventory system.

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): The text or name to echo back.

### `reverse_string`
- **Description:** Reverse a string.
- **Parameters:**
    - `input` (string): The string to reverse.

### `get_products`
- **Description:** Get all products from the inventory collection.

### `get_product_by_id`
- **Description:** Get a specific product by its ID.
- **Parameters:**
    - `id` (string): Product ID.

### `seed`
- **Description:** Seed the database with sample products.

### `reset`
- **Description:** Clear the inventory collection.

### `check_db`
- **Description:** Check the Firestore connection status.

### `get_root`
- **Description:** Get a welcome message from the API.

### `delete_product`
- **Description:** Delete a product by ID.
- **Parameters:**
    - `id` (string): Product ID.

### `update_product`
- **Description:** Update a product. Data must be a JSON string.
- **Parameters:**
    - `id` (string): Product ID.
    - `data` (string): JSON string of fields to update.

### `inventory_report`
- **Description:** Generates a full inventory report.

### `recommend_menu`
- **Description:** Recommends a menu for Keith.


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
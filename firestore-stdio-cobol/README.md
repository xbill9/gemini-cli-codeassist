# Firestore Stdio COBOL Server

A Model Context Protocol (MCP) server implemented in **COBOL** (GnuCOBOL) using the `mcpc` library via C bindings. This server communicates over `stdio` and acts as a **Firestore Inventory Management** system.

## Overview

This project provides an MCP server named `firestore-stdio-cobol` (binary: `firestore-server`) that allows managing a product inventory stored in Google Cloud Firestore. It writes structured JSON logs to `stderr`, ensuring that the `stdout` stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

-   **GnuCOBOL Compiler** (`cobc`)
-   **C Compiler** (GCC, Clang, etc.) supporting C17
-   **Make**
-   **Python 3** (for testing)
-   **curl** (for HTTP requests)
-   **Google Cloud SDK (`gcloud`)** (for authentication and project configuration)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-cobol
    # Initialize the mcpc submodule
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make firestore-server
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the `stdio` communication.

### Authentication & Setup

The server uses the `gcloud` CLI to automatically fetch the active project ID and access token.

1.  **Login to Google Cloud:**
    ```bash
    gcloud auth login
    ```
2.  **Set your project:**
    ```bash
    gcloud config set project YOUR_PROJECT_ID
    ```
3.  **Ensure Firestore is enabled** in your project.

### Running Manually

To run the server manually (starts listening for JSON-RPC on `stdin`):
```bash
./firestore-server
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look like this:

```json
{
  "mcpServers": {
    "firestore-stdio-cobol": {
      "command": "/absolute/path/to/firestore-stdio-cobol/firestore-server",
      "args": []
    }
  }
}
```

*Note: Always use absolute paths in MCP configurations.*

## Tools

### General
-   **`greet`**: Get a simple greeting (legacy/demo).
    -   `param` (string): Greeting parameter.
-   **`get_root`**: Get a welcome message from the Cymbal Superstore Inventory API.
-   **`check_db`**: Checks if the inventory database is accessible and running.

### Inventory Management
-   **`get_products`**: Get a list of all products from the inventory database.
-   **`get_product_by_id`**: Get a single product by its ID.
    -   `id` (string): The ID of the product.
-   **`seed`**: Seed the inventory database with initial sample products.
-   **`reset`**: Clear all products from the inventory database (Use with caution!).

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build COBOL server:** `make firestore-server`
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
- `firestore_client.c`: C implementation of Firestore REST API client using `curl`.
- `Makefile`: Build configuration for all components.
- `test_server_cobol.py`: Integration tests for the COBOL server.
- `mcpc/`: The `mcpc` library source code.
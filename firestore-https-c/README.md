# MCP HTTPS C Server (Firestore)

A Model Context Protocol (MCP) server implemented in C using the `mcpc` library. This server manages a product inventory in Google Cloud Firestore and communicates over HTTP (TCP Port 8080).

## Overview

This project provides a Firestore MCP server named `firestore-https-c`. It allows managing products in a Firestore collection named `inventory`. It writes structured JSON logs to stderr, while the JSON-RPC messages are handled over the TCP connection.

### Tools
- `greet`: Basic greeting tool.
- `list_products`: List all products in the inventory.
- `get_product`: Get a product by ID.
- `add_product`: Add a new product (name, price, quantity, imgfile).
- `update_product`: Update an existing product.
- `delete_product`: Delete a product by ID.
- `find_products`: Find products by name.
- `batch_delete`: Delete multiple products by ID (comma-separated).
- `get_system_info`: Get system information (OS, Kernel, etc.).
- `get_server_info`: Get information about this MCP server.
- `get_current_time`: Get current UTC time.

## Prerequisites

-   **C Compiler** (GCC, Clang, etc.) supporting C17.
-   **Make**
-   **libcurl** (development headers)
-   **gcloud CLI** (for authentication in local development)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-c
    # Initialize the mcpc submodule
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make
    ```

## Usage

This server starts a TCP listener on port **8080**.

To run the server:
```bash
./server
```

### Configuration for MCP Clients

If you are adding this to an MCP client config that supports HTTP/TCP transport:

```json
{
  "mcpServers": {
    "firestore-https-c": {
      "url": "http://localhost:8080"
    }
  }
}
```

*Note: The `mcpc` library handles the JSON-RPC over HTTP.*

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build:** `make`
- **Lint code:** `make lint`
- **Run tests:** `make test` (requires Python 3 and a configured GCP project)
- **Format code:** `make format`
- **Clean artifacts:** `make clean`

## Project Structure

- `main.c`: Entry point using `mcpc` and `libcurl` for Firestore.
- `Makefile`: Build configuration.
- `mcpc/`: The C MCP library submodule.
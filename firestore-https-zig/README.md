# MCP HTTPS Zig Server (Firestore Integration)

A Model Context Protocol (MCP) server implemented in Zig that integrates with Google Cloud Firestore. This server communicates over **HTTP** (defaulting to port 8080) and provides tools to manage a product inventory.

## Overview

This project provides an MCP server named `firestore-https-zig` that manages a "Cymbal Superstore" inventory stored in Firestore.
- **Transport:** HTTP (custom implementation, listening on 0.0.0.0:8080).
- **Database:** Google Cloud Firestore (requires authentication via `gcloud` or Metadata server).
- **Logging:** Structured JSON logs are written to `stderr`.

## Prerequisites

-   **Zig Compiler** (0.15.2 or later)
-   **Google Cloud Project** with Firestore enabled.
-   **Authentication:** The server uses `gcloud auth print-access-token` locally or the Compute Metadata server in GCP environments.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-zig
    ```

2.  **Build the server:**
    ```bash
    zig build -Doptimize=ReleaseSafe
    ```
    Or using `make`:
    ```bash
    make release
    ```

## Configuration

The server requires the following environment variables:
- `GCLOUD_PROJECT` or `GOOGLE_CLOUD_PROJECT`: Your Google Cloud Project ID.

Example:
```bash
export GCLOUD_PROJECT=my-mcp-project
./server
```

## Usage

To run the server:
```bash
./server
```

The server will start listening on `0.0.0.0:8080`.

### Interacting with the Server

MCP JSON-RPC messages are sent as the body of HTTP POST requests.

**Example: Get Products**

```bash
curl -X POST http://localhost:8080 \
     -H "Content-Type: application/json" \
     -d '{
       "jsonrpc": "2.0",
       "id": 1,
       "method": "tools/call",
       "params": {
         "name": "get_products",
         "arguments": {}
       }
     }'
```

## Tools

### Firestore Inventory Tools
- `get_products`: Get a list of all products from the inventory database.
- `get_product_by_id`: Get a single product by its ID. (Parameters: `id` (string))
- `seed`: Seed the inventory database with initial products.
- `reset`: Clears all products from the inventory database.
- `check_db`: Checks if the inventory database is running/connected.

### General Tools
- `get_root`: Get a greeting from the Cymbal Superstore Inventory API.
- `greet`: Get a greeting from a local http server. (Parameters: `param` (string))

## Development

- **Build:** `zig build` or `make build`
- **Release:** `zig build -Doptimize=ReleaseSafe` or `make release`
- **Test:** `zig build test` and `python3 test_server.py`
- **Format:** `make format`
- **Lint:** `make lint`

## Project Structure

- `src/main.zig`: Entry point, tool registration, and HTTP transport.
- `src/firestore.zig`: Firestore client implementation using `curl` and JSON-RPC.
- `build.zig`: Zig build configuration.
- `build.zig.zon`: Zig package manager configuration.
- `Makefile`: Convenience commands.
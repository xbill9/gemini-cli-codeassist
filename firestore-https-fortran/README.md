# Firestore HTTP Fortran Server

A Model Context Protocol (MCP) server implemented in **Fortran** using the `mcpc` library via C bindings. This server communicates over **HTTP** and acts as a **Firestore Inventory Management** system.

## Overview

This project provides an MCP server named `firestore-https-fortran` (binary: `firestore-https-server`) that allows managing a product inventory stored in Google Cloud Firestore. It exposes an MCP endpoint over HTTP, making it suitable for deployment as a web service or for use with MCP clients that support HTTP/SSE. It writes structured JSON logs to stderr.

## Prerequisites

-   **Fortran Compiler** (e.g., `gfortran`)
-   **C Compiler** (GCC, Clang, etc.) supporting C17
-   **Make**
-   **Python 3** (for testing)
-   **curl** (Runtime dependency: used for HTTP requests)
-   **Google Cloud SDK (`gcloud`)** (Runtime dependency: used for authentication and project configuration)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-fortran
    # Initialize the mcpc submodule
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    # Debug build
    make firestore-https-server
    
    # Release build (optimized)
    make release
    ```

## Docker Support

The project includes a `Dockerfile` for building a containerized version of the server.

> **Note:** The current `Dockerfile` produces a minimal image with the server binary. Ensure the runtime environment (or a modified Dockerfile) provides `curl` and `gcloud` (or compatible replacements) as the server relies on them for operations.

1.  **Build the image:**
    ```bash
    docker build -t firestore-https-fortran .
    ```

2.  **Run the container:**
    ```bash
    docker run -p 8080:8080 -e PORT=8080 firestore-https-fortran
    ```

## Deployment

### Google Cloud Run

The project is configured for deployment to Google Cloud Run via Cloud Build (`cloudbuild.yaml`).

1.  **Submit build:**
    ```bash
    gcloud builds submit . --config cloudbuild.yaml
    ```
    Or use the Makefile shortcut:
    ```bash
    make deploy
    ```

This will build the Docker image, push it to GCR, and deploy it to Cloud Run (configured for `us-central1`, unauthenticated access).

## Usage

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

To run the server manually:
```bash
PORT=8080 ./firestore-https-server
```
The server will start listening on the specified port (default is 8080 if not specified by environment variable `PORT`).

### Configuration for MCP Clients

Configure your MCP client to connect via HTTP to the server's URL.

```json
{
  "mcpServers": {
    "firestore-https-fortran": {
      "httpUrl": "http://127.0.0.1:8080/mcp"
    }
  }
}
```

## Tools

### General
-   `greet`: Get a greeting.
    -   `param` (string): Greeting parameter.
-   `check_db`: Checks if the inventory database is accessible and running.
-   `get_root`: Get a welcome message from the Cymbal Superstore Inventory API.

### Inventory Management
-   `get_products`: Get a list of all products from the inventory database.
-   `get_product_by_id`: Get a single product by its ID.
    -   `id` (string): The ID of the product.
-   `seed`: Seed the inventory database with initial sample products.
-   `reset`: Clear all products from the inventory database (Use with caution!).

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build Fortran server:** `make firestore-https-server`
- **Lint C code:** `make lint`
- **Run tests:** `make test` (requires Python 3)
- **Format C code:** `make format`
- **Check all:** `make check`
- **Clean artifacts:** `make clean`

## Project Structure

-   `server.f90`: The main Fortran implementation of the server and tool definitions.
-   `c_helpers.c`: C helper functions for logging and bridging.
-   `firestore_client.c`: C implementation of Firestore REST API client using `curl`.
-   `Makefile`: Commands for building C and Fortran binaries.
-   `test_server_fortran.py`: Integration tests for the server.
-   `mcpc/`: The C MCP library submodule.

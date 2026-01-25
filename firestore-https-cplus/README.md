# MCP HTTPS C++ Server with Firestore

A Model Context Protocol (MCP) server implemented in C++ using the `cpp-mcp` library. This server integrates with Google Cloud Firestore to manage a product inventory, exposing tools to search, retrieve, seed, and reset product data over HTTP/SSE.

## Overview

This project provides an MCP server named `mcp-https-cplus` that listens on `0.0.0.0:8080` (configurable). It serves as a backend for managing an inventory system, demonstrating how to build C++ MCP servers with real-world integrations.

## Prerequisites

-   **C++ Compiler** (GCC, Clang, etc.) supporting C++17.
-   **Make**
-   **Python 3** (for running integration tests)
-   **Google Cloud Project** with Firestore enabled (Native mode).
-   **Google Cloud CLI (`gcloud`)** installed and authenticated (for local development).

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-cplus
    # Initialize the submodules
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make
    ```

## Configuration & Authentication

The server requires a Google Cloud Project ID and a valid Access Token to communicate with Firestore.

### Environment Variables

-   `PORT`: Port to listen on (default: `8080`).
-   `GOOGLE_CLOUD_PROJECT`: Your Google Cloud Project ID.
    -   If not set, the server attempts to retrieve it via `gcloud config get-value project`.

### Authentication

-   **Local Development:**
    Ensure you are logged in with `gcloud`:
    ```bash
    gcloud auth login
    gcloud config set project YOUR_PROJECT_ID
    ```
    The server attempts to fetch an access token using `gcloud auth print-access-token` if not running in a cloud environment.

-   **Cloud Run / Compute Engine:**
    The server automatically uses the Metadata Server to obtain the project ID and access token for the attached Service Account. Ensure the Service Account has **Cloud Datastore User** role.

## Usage

To run the server manually:

```bash
export GOOGLE_CLOUD_PROJECT=your-project-id
./server
```

The server will start listening on `http://0.0.0.0:8080` (or the port specified by `PORT`).

### Configuration for MCP Clients

Add this server to your MCP client configuration (e.g., `claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "mcp-https-cplus": {
      "url": "http://localhost:8080/sse",
      "env": {
        "GOOGLE_CLOUD_PROJECT": "your-project-id"
      }
    }
  }
}
```

## Tools

The server exposes the following tools:

### `greet`
-   **Description:** Get a greeting from the local server.
-   **Parameters:** `param` (string) - The text to echo back.

### `get_products`
-   **Description:** Get a list of all products from the inventory database.
-   **Parameters:** None.

### `get_product_by_id`
-   **Description:** Get a single product from the inventory database by its ID.
-   **Parameters:** `id` (string) - The ID of the product.

### `search`
-   **Description:** Search for products in the inventory database by name.
-   **Parameters:** `query` (string) - The search query to filter products by name.

### `seed`
-   **Description:** Seed the inventory database with initial product data.
-   **Parameters:** None.

### `reset`
-   **Description:** Clear all products from the inventory database.
-   **Parameters:** None.

### `get_root`
-   **Description:** Get a greeting from the Cymbal Superstore Inventory API.
-   **Parameters:** None.

## Development

-   **Build:** `make`
-   **Debug Build:** `make debug`
-   **Run tests:** `make test` (Requires Python 3 and valid GCP credentials)
-   **Lint:** `make lint`
-   **Format:** `make format`
-   **Clean:** `make clean`
-   **Update Submodules:** `make submodule-update` (Pulls latest cpp-mcp)

## Deployment

The project includes a `cloudbuild.yaml` file for Google Cloud Build.

-   **Deploy:** `make deploy` (Submits build to Google Cloud Build)

Ensure you have the necessary permissions and APIs enabled on your Google Cloud project.
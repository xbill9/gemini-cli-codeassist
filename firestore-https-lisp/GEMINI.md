# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Common Lisp based Model Context Protocol (MCP) server** that integrates with **Google Cloud Firestore**. It exposes tools to manage a "Cymbal Superstore Inventory" database over **HTTPS (using Server-Sent Events - SSE)** for integration with MCP clients (such as Claude Desktop or Gemini clients), utilizing the `40ants/mcp` system.

The server implements tools for inventory management, including listing products, seeding the database, and basic utilities.

## Key Technologies

*   **Language:** Common Lisp (SBCL)
*   **Library:** `40ants/mcp`
*   **Database:** Google Cloud Firestore
*   **Transport:** HTTP/SSE (Server-Sent Events)
*   **Authentication:** Google Application Default Credentials (ADC)
*   **Package Manager:** Quicklisp / Ultralisp

## Tools

The server exposes the following tools via the `user-tools` API:

*   **`greet`**: A simple Hello World tool.
*   **`get_products`**: Fetches all products from the Firestore inventory collection.
*   **`get_product_by_id`**: Fetches a single product by its ID.
*   **`seed`**: Seeds the inventory database with sample products.
*   **`reset`**: Clears all products from the inventory database.
*   **`get_root`**: Returns a welcome message from the Cymbal Superstore Inventory API.
*   **`check_db`**: Verifies if the Firestore database is reachable.
*   **`add`**: A basic arithmetic tool.

## Development Setup

1.  **Prerequisites:**
    *   A Common Lisp implementation (e.g., SBCL).
    *   Quicklisp installed.
    *   Google Cloud SDK (`gcloud`) configured for authentication (if running locally).
    *   `GOOGLE_CLOUD_PROJECT` environment variable set.

2.  **Install Dependencies:**
    ```bash
    make deps
    ```

## Running the Server

### Using Make (Preferred)

```bash
make deps   # Install dependencies
make build  # Build the binary 'mcp-server'
./mcp-server
```

### Configuration

*   **Port:** Default is `8080`. Override with `PORT` environment variable.
*   **Project ID:** Uses `GOOGLE_CLOUD_PROJECT` or metadata service.
*   **Endpoints:**
    *   `GET /` or `/mcp`: SSE endpoint for events.
    *   `POST /` or `/mcp`: Endpoint for JSON-RPC requests.

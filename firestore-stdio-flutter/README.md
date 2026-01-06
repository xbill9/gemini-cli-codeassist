# Firestore MCP Server (Dart)

This is a **Dart-based Model Context Protocol (MCP) server** that interfaces with Google Cloud Firestore. It is designed to expose inventory management tools over standard input/output (stdio) for integration with MCP clients.

## Key Technologies

*   **Language:** Dart
*   **SDK:** `mcp_dart` (Model Context Protocol SDK for Dart)
*   **Database:** Google Cloud Firestore (via `firedart`)
*   **Configuration:** `dotenv`

## Prerequisites

*   Dart SDK (version 3.10.0 or later)
*   A Google Cloud Project with Firestore enabled.

## Setup

1.  **Get dependencies:**
    ```bash
    dart pub get
    ```

2.  **Configuration:**
    Create a `.env` file in the root directory (or set environment variables) with your Google Cloud Project ID:

    ```env
    FIRESTORE_PROJECT_ID=your-project-id
    # or
    GOOGLE_CLOUD_PROJECT=your-project-id
    ```

    *Note: `firedart` may require additional authentication setup depending on your environment (e.g., service account key).*

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
dart run bin/firestore_stdio_flutter.dart
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Tools

The server exposes the following tools for the "Cymbal Superstore Inventory API":

*   `get_products`: Returns a list of all products in the inventory.
*   `get_product_by_id`: Returns details for a single product by its ID.
    *   **Arguments:** `id` (string, required)
*   `seed`: Seeds the inventory database with sample product data (Apples, Bananas, etc.).
*   `reset`: Clears all products from the inventory database.
*   `get_root`: Returns a welcome message from the API.
*   `check_db`: Checks if the connection to the Firestore database is active.

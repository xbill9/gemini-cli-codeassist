# firestore-https-flutter

A Dart package implementing a **Model Context Protocol (MCP)** server for accessing Cloud Firestore. It supports HTTP transport.

## Prerequisites

-   Dart SDK >= 3.10.4
-   A Google Cloud Project with Firestore enabled.
-   `GOOGLE_CLOUD_PROJECT` environment variable set to your Google Cloud project ID.
-   Google Cloud credentials configured (e.g. via `gcloud auth application-default login`).

## Usage

Run the server over HTTP (SSE):

```bash
dart run bin/firestore_https_flutter.dart --port 8080 --host localhost --path /mcp
```

## Tools

The server exposes the following MCP tools for managing a "Cymbal Superstore Inventory":

-   **greet**: Get a greeting (test tool).
    -   Arguments: `name` (string)
-   **get_products**: Get a list of all products from the `inventory` collection.
-   **get_product_by_id**: Get a single product by its ID.
    -   Arguments: `id` (string)
-   **search**: Search for products in the inventory by name (case-insensitive).
    -   Arguments: `query` (string)
-   **seed**: Seed the `inventory` collection with sample data (Apples, Bananas, etc.).
-   **reset**: Clear all documents from the `inventory` collection.
-   **get_root**: Get a welcome message from the API.

## Development

### Running Tests

```bash
dart test
```

### Analysis

```bash
dart analyze
```

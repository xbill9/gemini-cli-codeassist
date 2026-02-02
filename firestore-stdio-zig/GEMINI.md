# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Zig-based Model Context Protocol (MCP) server** named `firestore-stdio-zig`. It uses the `mcp` Zig library (managed via Zig Package Manager) to implement the protocol. It exposes tools to interact with Google Cloud Firestore over standard input/output (stdio) and logs structured JSON to stderr.

## Key Technologies

*   **Language:** Zig (v0.15.2+)
*   **Library:** `mcp` (Zig library, fetched via `build.zig.zon`)
*   **Build System:** `zig build`
*   **Automation:** `Makefile` for common tasks.
*   **Communication:** Standard Input/Output (stdio) for MCP; `stderr` for logging.
*   **Database:** Google Cloud Firestore (via REST API using `curl`).
*   **Authentication:** Application Default Credentials (via `gcloud auth print-access-token` for local dev).

## Project Structure

*   `src/main.zig`: The entry point. Initializes the server, defines tools, and handles the event loop.
*   `src/firestore.zig`: Firestore client implementation using `curl`.
*   `build.zig`: Build configuration.
*   `build.zig.zon`: Package manifest.
*   `Makefile`: Shortcuts for build, test, clean.
*   `test_server.py`: Python script for integration testing.

## Implemented Tools

### `get_products`
*   **Description:** Get a list of all products from the inventory database.

### `get_product_by_id`
*   **Description:** Get a single product by ID.
*   **Parameters:** `id` (string).

### `seed`
*   **Description:** Seed the inventory database with sample products.

### `reset`
*   **Description:** Clears all products from the inventory database.

### `check_db`
*   **Description:** Checks if the inventory database is running/connected.

### `get_root`
*   **Description:** Get a greeting from the API.

## API Usage (mcp library)

The project uses the `mcp` library imported via `@import("mcp")`.

## Build & Development

```bash
make build && ./server
```

*   **Input:** JSON-RPC 2.0 messages via `stdin`.
*   **Output:** JSON-RPC 2.0 messages via `stdout`.
*   **Logs:** JSON formatted logs via `stderr`.

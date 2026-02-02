# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Zig-based Model Context Protocol (MCP) server** named `firestore-https-zig`. It implements an inventory management system backed by **Google Cloud Firestore**.
It uses a custom **HTTP Transport** listening on port 8080.

## Key Technologies

*   **Language:** Zig (v0.15.2+)
*   **Library:** `mcp` (Zig library, fetched via `build.zig.zon`)
*   **Database:** Google Cloud Firestore (REST API via `curl` subprocesses)
*   **Build System:** `zig build`
*   **Automation:** `Makefile` for common tasks.
*   **Communication:** HTTP (port 8080) for MCP messages; `stderr` for structured JSON logging.
*   **Testing:** Zig built-in testing framework (`zig build test`) and Python integration script (`test_server.py`).

## Project Structure

*   `src/main.zig`: The entry point. Initializes the server, registers tools, and implements `HttpServerTransport`.
*   `src/firestore.zig`: Firestore client. Handles authentication (via `gcloud` or Metadata server) and CRUD operations for products.
*   `build.zig`: Build configuration for compiling the Zig executable and linking dependencies.
*   `build.zig.zon`: Package manifest declaring the `mcp` dependency.
*   `Makefile`: Shortcuts for build, test, clean, format, and lint commands.
*   `test_server.py`: Python script for integration testing the running server.

## Implemented Tools

### Firestore Management
*   `get_products`: List all products.
*   `get_product_by_id`: Fetch a specific product by its ID.
    *   `id` (string): The ID of the product.
*   `seed`: Populate the database with sample products.
*   `reset`: Delete all products from the database.
*   `check_db`: Verify connectivity to the database.

### Misc
*   `get_root`: Simple API greeting.
*   `greet`: Echo greeting.
    *   `param` (string): Greeting parameter.

## Configuration

Requires environment variables for Firestore:
*   `GCLOUD_PROJECT` or `GOOGLE_CLOUD_PROJECT`: Target Google Cloud Project ID.

## API Usage (mcp library)

The project uses the `mcp` library imported via `@import("mcp")`.
*   Custom `HttpServerTransport` maps HTTP POST requests to MCP `Transport` methods.
*   Tools are registered using `server.addTool`.

## Build & Development

*   `make build`: Compile the server.
*   `make test`: Run unit and integration tests.
*   `./server`: Run the compiled binary (requires env vars).

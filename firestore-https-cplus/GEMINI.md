# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **C++ based Model Context Protocol (MCP) server** named `mcp-https-cplus`, implemented using the `cpp-mcp` library. It exposes tools to manage a product inventory stored in **Google Cloud Firestore**. It communicates over HTTP/SSE (listening on `0.0.0.0:8080`) and logs structured JSON to stderr.

## Key Technologies

*   **Language:** C++ (C++17 Standard)
*   **Library:** `cpp-mcp` (embedded in `cpp-mcp/` directory)
*   **Database:** Google Cloud Firestore (via REST API using `httplib`)
*   **Build System:** `make`
*   **Communication:** HTTP/SSE (port 8080) for MCP; `stderr` for logging.
*   **Testing:** Python 3 (`test_server.py`, `test_firestore.py`)
*   **Deployment:** Google Cloud Build (`cloudbuild.yaml`)

Do Not use stdio transport for this project

## Project Structure

*   `main.cpp`: The entry point. Configures the `mcp::server` and registers tools.
*   `firestore_client.hpp/cpp`: A custom Firestore client handling authentication (Metadata server or `gcloud`) and REST API calls.
*   `logger.hpp`: Logging utility for structured JSON logging.
*   `Makefile`: Build configuration.
*   `cloudbuild.yaml`: Google Cloud Build configuration.
*   `test_server.py`: Python script for integration testing the server.
*   `test_firestore.py`: Tests for the Python Firestore client wrapper (used in tests).
*   `cpp-mcp/`: The C++ MCP library source code (submodule).

## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local HTTP server.
*   **Parameters:** `param` (string)

### `get_products`
*   **Description:** Get a list of all products from the inventory.
*   **Parameters:** None

### `get_product_by_id`
*   **Description:** Get a single product by its ID.
*   **Parameters:** `id` (string)

### `search`
*   **Description:** Search for products by name.
*   **Parameters:** `query` (string)

### `seed`
*   **Description:** Seed the inventory database with sample products.
*   **Parameters:** None

### `reset`
*   **Description:** Clear all products from the inventory database.
*   **Parameters:** None

### `get_root`
*   **Description:** Get a greeting from the API.
*   **Parameters:** None

## API Usage (cpp-mcp)

The project uses the `cpp-mcp` library. Key components:

*   **mcp::server**: The main server class.
*   **mcp::tool_builder**: Fluent API for creating tool definitions.
*   **nlohmann::json**: Used for JSON manipulation.

## Authentication & Configuration

*   **Project ID:** Read from `GOOGLE_CLOUD_PROJECT` env var or `gcloud config`.
*   **Access Token:** Fetched via Metadata Server (Cloud Run) or `gcloud auth print-access-token` (Local).
*   **Port:** Configurable via `PORT` env var (default 8080).

## Build & Development

*   `make`: Build release binary (`server`).
*   `make debug`: Build with debug symbols.
*   `make test`: Run integration tests.
*   `make check`: Run lint and test.
*   `make clean`: Remove artifacts.
*   `make deploy`: Submit to Cloud Build.

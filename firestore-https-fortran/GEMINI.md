# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Fortran-based Model Context Protocol (MCP) server** named `firestore-https-fortran`, implemented using the `mcpc` library via C bindings (`iso_c_binding`). It functions as a **Firestore Inventory Management System**, exposing tools to list, query, seed, and reset an inventory database stored in Google Cloud Firestore.

## Key Technologies

*   **Language:** Fortran (2008+ with `iso_c_binding`), C (C17)
*   **Library:** `mcpc` (embedded in `mcpc/` directory)
*   **External CLIs:** `curl` (for HTTP requests), `gcloud` (for authentication)
*   **Build System:** `make`, `Docker`, `Google Cloud Build`
*   **Compiler:** `gfortran` (Fortran), `cc` (C)
*   **Communication:** HTTP (SSE) via `mcpc` TCP server; `stderr` for JSON structured logging.
*   **Testing:** Python 3 (`test_server_fortran.py`)
*   **Deployment:** Google Cloud Run

## Project Structure

*   `server.f90`: The main entry point. Defines the MCP server using `mcpc_server_new_tcp`, registers tools, and bridges to C implementations.
*   `firestore_client.c`: C implementation of the Firestore REST client. Uses `popen` to call `curl` and `gcloud`. Parses JSON responses using `mjson`.
*   `c_helpers.c`: C helper functions for logging and bridging macros.
*   `Makefile`: Build configuration.
*   `Dockerfile`: Multi-stage Docker build definition.
*   `cloudbuild.yaml`: Google Cloud Build configuration for deploying to Cloud Run.
*   `test_server_fortran.py`: Python script for integration testing over HTTP.
*   `mcpc/`: The `mcpc` library source code (submodule).

## Implemented Tools

### General
*   `greet`: Get a simple greeting.
    *   `param` (string): Greeting parameter.
*   `check_db`: Check if the Firestore database is configured and accessible.
*   `get_root`: Get a welcome message from the API.

### Inventory
*   `get_products`: List all products in the `/inventory` collection.
*   `get_product_by_id`: Retrieve a specific product by its document ID.
    *   `id` (string): Product ID.
*   `seed`: Populate the database with sample data.
*   `reset`: Delete all documents in the `/inventory` collection.

## API Usage (Fortran Interface)

The project defines an interface module `mcpc_interface` in `server.f90` to bind to `mcpc` C functions and the local `firestore_client.c` functions.

*   **Server Initialization:** `mcpc_server_new_tcp`
*   **Tool Registration:** `mcpc_tool_new2`, `mcpc_server_add_tool`
*   **Callback Handling:** Tools have associated Fortran callbacks (e.g., `get_products_cb`) that delegate to C functions (e.g., `impl_get_products`).
*   **Firestore Interaction:** The `impl_*` functions in `firestore_client.c` handle all HTTP/JSON logic.

## Build & Development

The `Makefile` supports the following targets:

*   `make firestore-https-server`: Builds the Fortran server binary.
*   `make test`: Runs integration tests (starts server on port 8080).
*   `make clean`: Removes build artifacts.

## Running the Server

```bash
PORT=8080 ./firestore-https-server
```

*   **Prerequisites:** `gcloud` must be authenticated and a project selected.
*   **Input/Output:** MCP over HTTP (POST to `/mcp`).
*   **Logs:** JSON formatted logs via `stderr`.

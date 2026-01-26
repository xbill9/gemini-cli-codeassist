# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **C++ based Model Context Protocol (MCP) server** named `firesdk-stdio-cplus`, implemented using the `cpp-mcp` library. It acts as an inventory management system backed by Google Cloud Firestore. It exposes multiple tools over standard input/output (stdio) to interact with the database and logs structured JSON to stderr.

## Key Technologies

*   **Language:** C++ (C++17 Standard)
*   **Library:** `cpp-mcp` (embedded in `cpp-mcp/` directory)
*   **Database:** Google Cloud Firestore (via REST API)
*   **Networking:** `httplib` (embedded) for REST calls.
*   **JSON:** `nlohmann::json` for data serialization.
*   **Build System:** `make` + `cmake`
*   **Communication:** Standard Input/Output (stdio) for MCP; `stderr` for logging.
*   **Testing:** Python 3 (`test_server.py`, `test_db.py`)

## Firestore C++ SDK
https://github.com/xbill9/firebase-cpp-sdk

## Firestore integrations
https://firebase.google.com/docs/firestore/firestore-for-rtdb?_gl=1*19tvj0r*_up*MQ..*_ga*MTkxMzU4OTQ1LjE3Njg3NjU4MjY.*_ga_CW55HF8NVT*czE3Njg3NjU4MjUkbzEkZzAkdDE3Njg3NjU4MjUkajYwJGwwJGgw

https://firebase.google.com/docs/firestore/use-rest-api?_gl=1*1itzago*_up*MQ..*_ga*MTkxMzU4OTQ1LjE3Njg3NjU4MjY.*_ga_CW55HF8NVT*czE3Njg3NjU4MjUkbzEkZzAkdDE3Njg3NjU4MjUkajYwJGwwJGgw

## Firestore C++ SDK Quirks & Design Decisions

This project currently uses a lightweight REST-based client (`firestore_client.hpp`) instead of the full [Firebase C++ SDK](https://firebase.google.com/docs/cpp/setup). This decision was made to address several specific characteristics (or "quirks") of working with the official SDK in a headless/server/CLI environment:

1.  **Mobile-First Focus:** The official C++ SDK is primarily optimized for Android and iOS. While desktop support exists, it is often geared towards development workflows (e.g., Unity editors) rather than production server environments.
2.  **Event Loop Requirement:** The SDK requires a continuous run loop (polling `App::Update`) to process callbacks and futures. This asynchronous model can be complex to integrate into a simple synchronous `stdin`/`stdout` CLI tool without careful threading.
3.  **Authentication:** On desktop, the SDK often requires specific setups (like `google-services.json` processing) or relies on user-facing auth flows. For a server tool, using standard Google Cloud Application Default Credentials (via `gcloud` or env vars) with the REST API is often more straightforward.
4.  **Build Complexity:** The full SDK pulls in significant dependencies (gRPC, Protobuf, etc.) and complex build chains. The REST approach allows for a much lighter build footprint using just `httplib` and `nlohmann/json`.

## Project Structure

*   `main.cpp`: The entry point. Implements a `StdioServer` class that handles the MCP protocol and Firestore REST calls.
    *   **Server Lifecycle:** `initialize`, `notifications/initialized`, `ping`.
    *   **Tool Management:** `tools/list`, `tools/call`.
    *   **Firestore Client:** Handles authentication (Env vars or gcloud) and CRUD operations via REST.
*   `Makefile`: Build configuration using CMake and Make.
*   `test_server.py`: General integration tests for the server tools.
*   `test_db.py`: Specific integration tests for database operations.
*   `cpp-mcp/`: The C++ MCP library source code (submodule).

## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local stdio server.
*   **Parameters:** `param` (string)

### `reverse_string`
*   **Description:** Reverse a string.
*   **Parameters:** `input` (string)

### `get_products`
*   **Description:** Get all products from the Firestore 'inventory' collection.

### `get_product_by_id`
*   **Description:** Get a specific product by its Firestore document ID.
*   **Parameters:** `id` (string)

### `seed`
*   **Description:** Seed the 'inventory' collection with sample product data.

### `reset`
*   **Description:** Clear the 'inventory' collection.

### `check_db`
*   **Description:** Check the Firestore connection status.

### `get_root`
*   **Description:** Returns a welcome message.

### `delete_product`
*   **Description:** Delete a product by ID.
*   **Parameters:** `id` (string)

### `update_product`
*   **Description:** Update a product. Data must be a JSON string.
*   **Parameters:** `id` (string), `data` (string - JSON string of fields to update)

### `inventory_report`
*   **Description:** Generates a full inventory report.

### `recommend_menu`
*   **Description:** Recommends a menu for Keith.

## API Usage (cpp-mcp)

The project uses the `cpp-mcp` library. Key components used in `main.cpp`:

*   **mcp::tool**: Represents an MCP tool definition.
*   **mcp::tool_builder**: Fluent API for creating tools.
*   **mcp::request / mcp::response**: Helpers for JSON-RPC messages.
*   **nlohmann::json**: (Aliased as `mcp::json`) Used for JSON manipulation.
*   **Firestore::Client**: Custom class in `main.cpp` for Firestore REST API interaction.

## Build & Development

The `Makefile` supports the following targets:

*   `make` (or `make all`): Builds the `server` binary (Release).
*   `make debug`: Builds with debug symbols.
*   `make test`: Runs integration tests (`test_server.py`).
*   `make check`: Runs lint and test.
*   `make lint`: Checks formatting.
*   `make format`: Applies formatting.
*   `make clean`: Removes build artifacts.

## Running the Server

```bash
./server
```

*   **Environment Variables:**
    *   `GOOGLE_CLOUD_PROJECT` or `FIREBASE_PROJECT_ID`: Required for Firestore.
    *   `FIRESTORE_ACCESS_TOKEN`: Optional. If not set, tries `gcloud auth print-access-token`.
*   **Input:** JSON-RPC 2.0 messages via `stdin`.
*   **Output:** JSON-RPC 2.0 messages via `stdout`.
*   **Logs:** JSON formatted logs via `stderr`.

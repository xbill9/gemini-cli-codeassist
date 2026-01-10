# Firestore MCP Server

A Model Context Protocol (MCP) server implemented in Swift that integrates with Google Cloud Firestore. It uses the streaming HTTP transport (SSE) and is built on the Hummingbird 2.0 web framework.

## Overview

This project provides an MCP server named `firestore-https-swift` that manages a product inventory in Firestore. It exposes several tools for querying and managing product data.

**Key Features:**
*   **Transport:** Uses Streaming HTTP (Server-Sent Events) for MCP communication.
*   **Firestore Integration:** Connects to Google Cloud Firestore with support for local service account credentials or Google Cloud Metadata Server (for Cloud Run/GCE).
*   **Concurrency:** Built on Swift's structured concurrency and `ServiceLifecycle`.
*   **SDK:** Powered by the [MCP Swift SDK](https://github.com/modelcontextprotocol/swift-sdk).
*   **Observability:** Uses `swift-log` with JSON formatting for structured logging.
*   **Deployment:** Ready for containerized deployment (Docker, Google Cloud Run).

## Prerequisites

- **Swift 6.0+** (Developed with Swift 6.2)
- **Linux** or **macOS**
- **Docker** (optional, for containerized build/run)
- **Google Cloud Project** with Firestore enabled (if using Firestore)

## Getting Started

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-swift
    ```

2.  **Install dependencies:
    ```bash
    make install
    ```

3.  **Build the project:
    ```bash
    make build
    # or for release build
    make release
    ```

## Configuration

The server can be configured via environment variables:

- `PORT`: The port to listen on (default: `8080`).
- `GOOGLE_APPLICATION_CREDENTIALS`: Path to a Google Cloud Service Account JSON key file. If not set, the server will attempt to use the Google Cloud Metadata Server (useful for Cloud Run/GCE). If neither is available, it falls back to a limited in-memory mode.

## Usage

### Local Development

Start the server:
```bash
make run
```
The server will start listening on `http://0.0.0.0:8080` by default.

### Docker

Build the Docker image:
```bash
docker build -t firestore-https-swift .
```

Run the container:
```bash
docker run -p 8080:8080 -e GOOGLE_APPLICATION_CREDENTIALS=/path/to/key.json -v /path/to/key.json:/path/to/key.json firestore-https-swift
```

### Make Commands

The `Makefile` provides several useful shortcuts:

- `make build`: Build the application (debug mode).
- `make release`: Build the application (release mode).
- `make run`: Run the application locally.
- `make test`: Run unit tests.
- `make lint`: Lint the code using `swift-format`.
- `make format`: Format the code using `swift-format`.
- `make check`: Run both lint and tests.
- `make clean`: Clean build artifacts.
- `make deploy`: Deploy to Google Cloud Run (requires `gcloud` CLI).

## API Endpoints

The server exposes the following endpoints:

### `GET /mcp`
Establishes an SSE connection.
- **Returns:** An event stream.
- **Headers:** The response includes an `Mcp-Session-Id` header identifying the session.
- **Events:** The server sends an `endpoint` event containing the session URL (e.g., `/mcp?sessionId=<UUID>`).

### `POST /mcp`
Endpoint to send JSON-RPC messages to the server.
- **Headers:** Requires `Mcp-Session-Id` header matching the active session.
- **Query Params:** Alternatively, `sessionId` can be passed as a query parameter (fallback).
- **Body:** The JSON-RPC message.

### `DELETE /mcp`
Terminates an active session.
- **Headers:** Requires `Mcp-Session-Id` header.

### `GET /status` & `GET /health`
Returns JSON information about the server and database status.

## Tools

### `greet`
- **Description:** A simple greeting tool.
- **Arguments:**
    - `name` (string, required): The name to greet.

### `get_products`
- **Description:** Get a list of all products from the inventory database.

### `get_product_by_id`
- **Description:** Get a single product from the inventory database by its ID.
- **Arguments:**
    - `id` (string, required): The ID of the product.

### `search`
- **Description:** Search for products in the inventory database by name.
- **Arguments:
    - `query` (string, required): The search query.

### `seed`
- **Description:** Seed the inventory database with sample products.

### `reset`
- **Description:** Clears all products from the inventory database.

### `get_root`
- **Description:** Get a greeting from the Cymbal Superstore Inventory API.

### `check_db`
- **Description:** Checks if the inventory database is running.

## Deployment

The project includes a `cloudbuild.yaml` for Google Cloud Build and a `deploy` target in the `Makefile`.

To deploy to Google Cloud Run:
```bash
make deploy
```
Ensure you have the Google Cloud SDK installed and authenticated.

## Project Structure

- `Sources/firestore-https-swift/`: Source code.
  - `main.swift`: Entry point, server configuration, and routing.
  - `Handlers.swift`: MCP tool implementations.
  - `FirestoreClient.swift`: Client for interacting with Firestore.
  - `GoogleAuth.swift`: Authentication logic for Google Cloud.
  - `SessionManager.swift`: Manages active MCP sessions.
  - `SSEServerTransport.swift`: Custom SSE transport implementation.
  - `JSONLogHandler.swift`: Custom log handler for JSON output.
  - `Models.swift`: Data models.
- `Tests/`: Unit tests.
- `Dockerfile`: Multi-stage Docker build configuration.
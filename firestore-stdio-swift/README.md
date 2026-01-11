# MCP Firestore Swift Server

A Model Context Protocol (MCP) server implemented in Swift that integrates with Google Cloud Firestore. This server exposes tools to manage a product inventory database.

## Overview

This project provides an MCP server named `inventory-server` (implemented in the `firestore-stdio-swift` package) that interacts with a Firestore database using the REST API. Built with **Swift 6**, it leverages structured concurrency and the official MCP Swift SDK.

**Implementation Details:**
- **MCP SDK:** Uses the official `modelcontextprotocol/swift-sdk`.
- **Concurrency:** Fully utilizes Swift 6 `async/await` and `actor` models.
- **Networking:** Employs `swift-server/async-http-client` for high-performance asynchronous HTTP requests.
- **Security:** Integrates `vapor/jwt-kit` for Google Service Account JWT authentication.
- **Lifecycle:** Uses `swift-server/swift-service-lifecycle` for robust process management and graceful shutdown.

**Key Features:**
*   **Firestore Integration:** Perform CRUD operations on a `inventory` collection.
*   **Authentication:** Securely connects using Google Service Account credentials.
*   **Transport:** Uses standard input/output (`stdio`) for MCP communication.
*   **Structured Logging:** Logs are formatted as JSON and sent to `stderr` to avoid interfering with the MCP protocol on `stdout`.
*   **Graceful Shutdown:** Managed by `swift-service-lifecycle`.

## Prerequisites

- **Swift 6.0+**
- **Google Cloud Platform Project** with Firestore enabled.
- **Service Account Key** (JSON file) with the `Cloud Datastore User` role.

## Setup

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-swift
    ```

2.  **Build the project:**
    ```bash
    make build
    ```

3.  **Configure Credentials:**
    The server requires the `GOOGLE_APPLICATION_CREDENTIALS` environment variable to be set to the path of your Service Account JSON key file to enable Firestore functionality. If not set, database tools will return errors.

    ```bash
    export GOOGLE_APPLICATION_CREDENTIALS="/path/to/service-account-key.json"
    ```

## Usage

### Testing with MCP Inspector

You can test the server using the MCP Inspector:

```bash
GOOGLE_APPLICATION_CREDENTIALS="/path/to/key.json" npx @modelcontextprotocol/inspector .build/debug/firestore-stdio-swift
```

### Configuration for MCP Clients

To use this server with an MCP client (like Claude Desktop), add the following to your configuration file (e.g., `claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "firestore-swift": {
      "command": "/absolute/path/to/firestore-stdio-swift/.build/release/firestore-stdio-swift",
      "args": [],
      "env": {
        "GOOGLE_APPLICATION_CREDENTIALS": "/absolute/path/to/service-account-key.json"
      }
    }
  }
}
```

## Exposed Tools

### `get_products`
- **Description:** Get a list of all products from the inventory database.

### `get_product_by_id`
- **Description:** Get a single product from the inventory database by its ID.
- **Parameters:**
    - `id` (string, required): The ID of the product to get.

### `seed`
- **Description:** Seed the inventory database with products.

### `reset`
- **Description:** Clears all products from the inventory database.

### `check_db`
- **Description:** Checks if the inventory database is running.

### `get_root`
- **Description:** Get a greeting from the Cymbal Superstore Inventory API.

## Development

- **Build:** `make build` (Debug build)
- **Release:** `make release` (Release build)
- **Run:** `make run` (Requires `GOOGLE_APPLICATION_CREDENTIALS`)
- **Test:** `make test`
- **Format:** `make format` (Requires `swift-format`)
- **Clean:** `make clean`

## Project Structure

- `Sources/firestore-stdio-swift/main.swift`: Entry point, server initialization, and lifecycle management.
- `Sources/firestore-stdio-swift/Handlers.swift`: Implementation of MCP tool handlers.
- `Sources/firestore-stdio-swift/FirestoreClient.swift`: Firestore REST API client.
- `Sources/firestore-stdio-swift/GoogleAuth.swift`: Google Service Account authentication logic.
- `Sources/firestore-stdio-swift/Models.swift`: Data models for Products and Firestore documents.
- `Sources/firestore-stdio-swift/JSONLogHandler.swift`: Custom logger for JSON output to `stderr`.
- `Sources/firestore-stdio-swift/MCPService.swift`: Bridges the MCP Server with the Swift Service Lifecycle.
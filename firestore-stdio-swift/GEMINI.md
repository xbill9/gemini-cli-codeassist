# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Swift-based Model Context Protocol (MCP) server** named `inventory-server` (implemented in the `firestore-stdio-swift` package). It integrates with Google Cloud Firestore to manage a product inventory database over standard input/output (stdio).

## Key Technologies

*   **Language:** Swift 6.0
*   **SDK:** [modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **Libraries:** 
    *   `swift-server/swift-service-lifecycle`: For graceful shutdown and structured concurrency management.
    *   `apple/swift-log`: For logging (configured via `JSONLogHandler` to output to `stderr`).
    *   `swift-server/async-http-client`: For Firestore REST API communication.
    *   `vapor/jwt-kit`: For Google Service Account authentication (JWT).
*   **Build System:** Swift Package Manager (SPM)

## Project Structure

*   `Package.swift`: Defines dependencies (`MCP`, `ServiceLifecycle`, `Logging`, `AsyncHTTPClient`, `JWTKit`) and the executable target.
*   `Sources/firestore-stdio-swift/main.swift`: The main entry point. Initializes logging, Firestore client, MCP server, and handles the service lifecycle.
*   `Sources/firestore-stdio-swift/Handlers.swift`: Contains the `listTools` and `callTool` implementations.
*   `Sources/firestore-stdio-swift/FirestoreClient.swift`: An actor-based client for interacting with the Firestore REST API.
*   `Sources/firestore-stdio-swift/GoogleAuth.swift`: Manages Google Service Account authentication and token refreshing.
*   `Sources/firestore-stdio-swift/Models.swift`: Data models for `Product` and Firestore-specific JSON structures.
*   `Sources/firestore-stdio-swift/JSONLogHandler.swift`: Ensures logs are written to `stderr` in JSON format.
*   `Sources/firestore-stdio-swift/MCPService.swift`: Bridges the `Server` with `ServiceLifecycle`.
*   `Tests/firestore-stdio-swiftTests/HandlersTests.swift`: Unit tests for the tool handlers.
*   `Makefile`: Development shortcuts for `build`, `run`, `test`, and `clean`.

## Exposed Tools

### `get_products`
*   **Description:** Get a list of all products from the inventory database.

### `get_product_by_id`
*   **Description:** Get a single product from the inventory database by its ID.
*   **Arguments:**
    *   `id` (string, required): The ID of the product to get.

### `seed`
*   **Description:** Seed the inventory database with a default set of products.

### `reset`
*   **Description:** Clears all products from the inventory database.

### `check_db`
*   **Description:** Checks if the inventory database client is correctly initialized.

### `get_root`
*   **Description:** Get a greeting from the Cymbal Superstore Inventory API.

## Development Workflows

### Environment Configuration
The server requires the `GOOGLE_APPLICATION_CREDENTIALS` environment variable to be set to the path of a Google Service Account JSON key file with `Cloud Datastore User` permissions. Without this, tools interacting with Firestore will fail.

### Makefile Commands
*   `make build`: Compiles the project using `swift build` (Debug mode).
*   `make release`: Compiles the project using `swift build -c release`.
*   `make run`: Executes the server using `swift run`. Requires `GOOGLE_APPLICATION_CREDENTIALS`.
*   `make test`: Runs unit tests.
*   `make format`: Formats the source code (requires `swift-format`).
*   `make clean`: Removes the `.build` directory.

### Logging
Logging is bootstrapped to `stderr` to avoid corrupting the MCP `stdio` transport:
```swift
LoggingSystem.bootstrap { label in
  JSONLogHandler(label: label)
}
```

## Swift MCP Best Practices

1.  **Logging to `stderr`:** Always log to `stderr`. Never use `print()` as it writes to `stdout`, which is reserved for MCP transport and will cause protocol errors.
2.  **Structured Concurrency:** Leverage Swift 6's `async/await` and `actor` models for safe concurrent operations.
3.  **Graceful Shutdown:** Use `ServiceLifecycle` to ensure resources (like `HTTPClient`) are shut down properly on `SIGTERM`.
4.  **Error Handling:** Return errors within the `CallTool.Result` (setting `isError: true`) instead of crashing the server.

## Swift MCP Developer Resources

*   **MCP Swift SDK (GitHub):** [https://github.com/modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **MCP Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
*   **Package Submission:** [https://swiftpackageindex.com/add-a-package](https://swiftpackageindex.com/add-a-package)

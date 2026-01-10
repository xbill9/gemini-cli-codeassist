# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Firestore MCP Server** implemented in Swift using the `swift-sdk`. It exposes tools over **Streaming HTTP (SSE)** using the Hummingbird web framework (v2.0).

## Key Technologies

*   **Language:** Swift 6.0+ (Developed with Swift 6.2)
*   **SDK:** `modelcontextprotocol/swift-sdk` (v0.10.2)
*   **Web Framework:** `hummingbird-project/hummingbird` (v2.0.0)
*   **Concurrency:** `swift-server/swift-service-lifecycle` (v2.6.0) for graceful shutdown and structured concurrency.
*   **Logging:** `apple/swift-log` (v1.6.0) with JSON formatting.
*   **Build System:** Swift Package Manager (SPM)
*   **Containerization:** Docker (Multi-stage build)

## Project Structure

*   `Package.swift`: Defines dependencies.
*   `Sources/firestore-https-swift/main.swift`: Application entry point. Sets up the Hummingbird router, `SessionManager`, and starts the server.
*   `Sources/firestore-https-swift/Handlers.swift`: Implements the MCP tools (`listTools`, `callTool`).
*   `Sources/firestore-https-swift/SessionManager.swift`: Actor that manages active `SSEServerTransport` instances by Session ID.
*   `Sources/firestore-https-swift/SSEServerTransport.swift`: Custom `Transport` implementation.
    *   Bridges Hummingbird's request/response model to the MCP SDK's `Transport` protocol.
    *   Manages an `AsyncStream<ServerSentEvent>` for outbound SSE messages.
    *   Manages an `AsyncThrowingStream<Data, Error>` for inbound JSON-RPC messages (via POST).
*   `Sources/firestore-https-swift/JSONLogHandler.swift`: Custom log handler for structured logging.
*   `Makefile`: Development shortcuts.
*   `Dockerfile`: Configuration for building the production image.
*   `cloudbuild.yaml`: Google Cloud Build configuration.

## Exposed Tools

### `greet`
*   **Description:** Get a greeting from the server.
*   **Arguments:**
    *   `param` (string, required): The name or parameter to greet.
*   **Response:** Returns the `param` value as text.

## HTTP Endpoints

### `GET /mcp`
Establishes an SSE connection.
- **Headers:** Response includes `Mcp-Session-Id`.
- **Events:** Sends an `endpoint` event with the session URL (e.g., `/mcp?sessionId=<UUID>`).

### `POST /mcp`
Endpoint to send JSON-RPC messages.
- **Headers:** Requires `Mcp-Session-Id` header.
- **Query Params:** Accepts `sessionId` as a fallback.
- **Body:** JSON-RPC message payload.

## Development Workflows

### Makefile Commands
*   `make build`: Build the application (debug).
*   `make release`: Build the application (release).
*   `make run`: Run the application locally (`0.0.0.0:8080`).
*   `make test`: Run unit tests.
*   `make lint`: Lint code with `swift-format`.
*   `make format`: Format code with `swift-format`.
*   `make check`: Run lint and tests.
*   `make clean`: Remove build artifacts.
*   `make deploy`: Deploy to Google Cloud Run.

## Swift MCP Implementation Details

1.  **Session Management:**
    *   `SessionManager` creates a new `SSEServerTransport` for each `GET /mcp` request.
    *   The transport is stored by a UUID.
    *   `POST /mcp` requests look up the transport using the session ID and inject data via `handlePostData()`.
    *   The `SessionCleanupService` ensures all sessions are disconnected on shutdown.

2.  **Concurrency:**
    *   The `Server` runs in a background `Task` for each session.
    *   `ServiceLifecycle` manages the main application run loop and graceful shutdown.

3.  **Error Handling:**
    *   Tools return `CallTool.Result` with `isError: true` for predictable failures (e.g., missing arguments).
    *   Critical server errors are logged and may terminate the session.

## Swift MCP Developer Resources

*   **MCP Swift SDK (GitHub):** [https://github.com/modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **MCP Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
*   **Hummingbird Documentation:** [https://hummingbird.codes/](https://hummingbird.codes/)
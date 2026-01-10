# Firestore MCP Server

A Model Context Protocol (MCP) server implemented in Swift using the streaming HTTP transport (SSE).

## Overview

This project provides a basic MCP server named `firestore-https-swift` that exposes a single tool: `greet`. It uses [Hummingbird 2.0](https://hummingbird.codes/) to provide an HTTP interface for the MCP protocol.

**Key Features:**
*   **Transport:** Uses Streaming HTTP (Server-Sent Events) for MCP communication.
*   **Concurrency:** Built on Swift's structured concurrency and `ServiceLifecycle`.
*   **SDK:** Powered by the [MCP Swift SDK](https://github.com/modelcontextprotocol/swift-sdk).
*   **Observability:** Uses `swift-log` with JSON formatting for structured logging.
*   **Deployment:** Ready for containerized deployment (Docker, Google Cloud Run).

## Prerequisites

- **Swift 6.0+** (Developed with Swift 6.2)
- **Linux** or **macOS**
- **Docker** (optional, for containerized build/run)

## Getting Started

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-swift
    ```

2.  **Install dependencies:**
    ```bash
    make install
    ```

3.  **Build the project:**
    ```bash
    make build
    # or for release build
    make release
    ```

## Usage

### Local Development

Start the server:
```bash
make run
```
The server will start listening on `http://0.0.0.0:8080` by default. You can override the port using the `PORT` environment variable.

### Docker

Build the Docker image:
```bash
docker build -t firestore-https-swift .
```

Run the container:
```bash
docker run -p 8080:8080 firestore-https-swift
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
- **Events:** The server sends an endpoint event containing the session ID in the query parameters (e.g., `/mcp?sessionId=<UUID>`).

### `POST /mcp`
Endpoint to send JSON-RPC messages to the server.
- **Headers:** Requires `Mcp-Session-Id` header matching the active session.
- **Query Params:** Alternatively, `sessionId` can be passed as a query parameter (fallback).
- **Body:** The JSON-RPC message.

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string, required): The name or parameter to greet.
- **Returns:** The string passed in `param`.

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
  - `SessionManager.swift`: Manages active MCP sessions.
  - `SSEServerTransport.swift`: Custom SSE transport implementation.
  - `JSONLogHandler.swift`: Custom log handler for JSON output.
- `Tests/`: Unit tests.
- `Dockerfile`: Multi-stage Docker build configuration.
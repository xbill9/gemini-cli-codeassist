# MCP HTTP Swift Server

A Model Context Protocol (MCP) server implemented in Swift using the streaming HTTP transport (SSE).

## Overview

This project provides a basic MCP server named `mcp-http-server` that exposes a single tool: `greet`. It uses [Hummingbird](https://github.com/hummingbird-project/hummingbird) to provide an HTTP interface for the MCP protocol.

**Key Features:**
*   **Transport:** Uses Streaming HTTP (Server-Sent Events) for MCP communication.
*   **Concurrency:** Built on Swift's structured concurrency and `ServiceLifecycle`.
*   **SDK:** Powered by the [MCP Swift SDK](https://github.com/modelcontextprotocol/swift-sdk).
*   **Web Framework:** Built with [Hummingbird 2.0](https://hummingbird.codes/).

## Prerequisites

- **Swift 6.0+** (or compatible Swift toolchain)
- **Linux** or **macOS**

## Getting Started

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-http-swift
    ```

2.  **Build the project:**
    
    ```bash
    make build
    # Executable will be at: .build/debug/mcp-stdio-swift
    ```

## Usage

Start the server:
```bash
make run
```
The server will start listening on `http://0.0.0.0:8080`.

### MCP Endpoints

- **GET `/mcp`**: Establishes an SSE connection. The server returns the `Mcp-Session-Id` header in the response.
- **POST `/mcp`**: Endpoint to send JSON-RPC messages. Requires the `Mcp-Session-Id` header.

### Configuration for MCP Clients

If you are adding this to an MCP client config that supports HTTP/SSE:

**Example URL:** `http://localhost:8080/mcp`

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string, required): The name or parameter to greet.
- **Returns:** The string passed in `param`.

## Development

- **Build:** `make build`
- **Run:** `make run`
- **Test:** `make test`
- **Clean:** `make clean`

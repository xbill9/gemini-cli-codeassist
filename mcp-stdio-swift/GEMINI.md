# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Swift-based Model Context Protocol (MCP) server** using the `swift-sdk`. It exposes tools over standard input/output (stdio) for integration with MCP clients.

## Key Technologies

*   **Language:** Swift 6.0
*   **SDK:** [modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **Libraries:** 
    *   `swift-server/swift-service-lifecycle`: For graceful shutdown and structured concurrency management.
    *   `apple/swift-log`: For logging (configured to output to `stderr` to avoid corrupting `stdio` transport).
*   **Build System:** Swift Package Manager (SPM)

## Project Structure

*   `Package.swift`: Defines dependencies (`swift-sdk`, `swift-service-lifecycle`, `swift-log`) and the executable target.
*   `Sources/mcp-stdio-swift/main.swift`: The main entry point. Initializes the server, handlers, and service group.
*   `Sources/mcp-stdio-swift/Handlers.swift`: Contains the `listTools` and `callTool` implementations.
*   `Sources/mcp-stdio-swift/MCPService.swift`: Bridges the `Server` with `ServiceLifecycle`.
*   `Makefile`: Development shortcuts.

## Exposed Tools

### `greet`
*   **Description:** Get a greeting from a local stdio server.
*   **Arguments:**
    *   `param` (string, required): The name or parameter to greet.
*   **Response:** Returns the `param` value as text.

## Development Workflows

### Makefile Commands
*   `make build`: Compiles the project using `swift build`.
*   `make run`: Executes the server using `swift run`.
*   `make test`: Runs unit tests (if any).
*   `make install`: Resolves Swift package dependencies.
*   `make clean`: Removes the `.build` directory.

### Logging
Logging is bootstrapped to `stderr`:
```swift
LoggingSystem.bootstrap { label in
    StreamLogHandler.standardError(label: label)
}
```
This is critical because the MCP protocol uses `stdout` for communication. Any data written to `stdout` that isn't a valid MCP message will cause protocol errors in the client.

## Development Setup

1.  **Install Swift:** Ensure Swift 6.0+ is installed.
2.  **Build:**
    ```bash
    make build
    ```

## Running the Server

```bash
make run
```

## Swift MCP Developer Resources

*   **MCP Swift SDK (GitHub):** [https://github.com/modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **MCP Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)

## TODO / Legacy Files
*   `Dockerfile`: Needs update to use a Swift base image (e.g., `swift:6.0-jammy`).
*   `cloudbuild.yaml`: Needs update for Swift build steps.
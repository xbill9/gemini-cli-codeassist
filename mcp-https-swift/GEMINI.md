# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Swift-based Model Context Protocol (MCP) server** using the `swift-sdk`. It exposes tools over **Streaming HTTP (SSE)** using the Hummingbird web framework.

## Key Technologies

*   **Language:** Swift 6.0
*   **SDK:** [modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **Libraries:** 
    *   `hummingbird-project/hummingbird`: For the HTTP server and SSE support.
    *   `swift-server/swift-service-lifecycle`: For graceful shutdown and structured concurrency management.
    *   `apple/swift-log`: For logging.
*   **Build System:** Swift Package Manager (SPM)

## Project Structure

*   `Package.swift`: Defines dependencies (`swift-sdk`, `hummingbird`, `swift-service-lifecycle`, `swift-log`).
*   `Sources/mcp-https-swift/main.swift`: The main entry point. Initializes the Hummingbird app and MCP sessions.
*   `Sources/mcp-https-swift/SSEServerTransport.swift`: Custom `Transport` implementation for SSE.
*   `Sources/mcp-https-swift/SessionManager.swift`: Manages active MCP sessions.
*   `Sources/mcp-https-swift/Handlers.swift`: Contains the `listTools` and `callTool` implementations.
*   `Makefile`: Development shortcuts.

## Exposed Tools

### `greet`
*   **Description:** Get a greeting from the server.
*   **Arguments:**
    *   `param` (string, required): The name or parameter to greet.
*   **Response:** Returns the `param` value as text.

## HTTP Endpoints

*   `GET /mcp`: Initialize an MCP session over SSE. Returns `Mcp-Session-Id` header.
*   `POST /mcp`: Send MCP messages to a session. Requires `Mcp-Session-Id` header.

## Development Workflows

### Makefile Commands
*   `make build`: Compiles the project using `swift build`.
*   `make run`: Executes the server using `swift run`.
*   `make test`: Runs unit tests.
*   `make clean`: Removes the `.build` directory.

## Swift MCP Best Practices

1.  **SSE Session Management:** Each SSE connection should represent a unique MCP session.
2.  **Structured Concurrency:** Leverage Swift 6's `async/await`.
3.  **Error Reporting:** Return informative error messages in `CallTool.Result`.


## Swift MCP Developer Resources

https://github.com/modelcontextprotocol/swift-sdk/blob/main/Sources/MCP/Base/Transports/HTTPClientTransport.swift
https://github.com/modelcontextprotocol/swift-sdk/blob/main/Sources/MCP/Server/Server.swift#L43

*   **MCP Swift SDK (GitHub):** [https://github.com/modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **MCP Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
package submission https://swiftpackageindex.com/add-a-package
https://github.com/SwiftPackageIndex/PackageList/issues/new/choose
https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/blob/main/CODE_OF_CONDUCT.md

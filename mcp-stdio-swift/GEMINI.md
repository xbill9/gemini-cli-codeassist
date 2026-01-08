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

## Swift MCP Best Practices

1.  **Logging to `stderr`:** Always log to `stderr` using `LoggingSystem.bootstrap`. Never use `print()` as it writes to `stdout`, which is reserved for the MCP protocol transport and will cause protocol errors.
2.  **Structured Concurrency:** Leverage Swift 6's `async/await` for tool handlers. Enable `StrictConcurrency` in `Package.swift` to catch data races at compile time.
3.  **Graceful Shutdown:** Use `swift-service-lifecycle` to handle `SIGTERM` and `SIGINT`, ensuring the server closes its transport and cleans up resources properly.
4.  **Input Validation:** Use the `Value` type and its helper methods to safely extract arguments from tool calls. Always validate required parameters and return clear error messages.
5.  **Tool Schemas:** Define clear JSON schemas for tools in `listTools` to ensure clients provide the correct input types.
6.  **Error Reporting:** Return informative error messages in `CallTool.Result` with `isError: true` rather than throwing fatal errors, which would crash the server and disconnect the client.
7.  **Modular Handlers:** Keep tool implementations in a separate `Handlers` struct or class to improve testability and keep `main.swift` focused on orchestration.

## Swift MCP Developer Resources

*   **MCP Swift SDK (GitHub):** [https://github.com/modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **MCP Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
package submission https://swiftpackageindex.com/add-a-package
https://github.com/SwiftPackageIndex/PackageList/issues/new/choose
https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/blob/main/CODE_OF_CONDUCT.md

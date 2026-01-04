# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Swift-based Model Context Protocol (MCP) server** using the `swift-sdk`. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients.

## Key Technologies

*   **Language:** Swift 6
*   **SDK:** `modelcontextprotocol/swift-sdk`
*   **Libraries:** 
    *   `swift-service-lifecycle` (for graceful shutdown)
    *   `swift-log` (for logging)
*   **Build System:** Swift Package Manager (SPM)
https://github.com/modelcontextprotocol/swift-sdk

## Project Structure

*   `Package.swift`: The package manifest defining dependencies and targets.
*   `Sources/mcp-stdio-swift/main.swift`: The entry point of the application. Initializes the MCP server and defines tools.
*   `Makefile`: Development shortcuts (build, run, clean).

## Development Setup

1.  **Install Swift:** Ensure Swift 6.0+ is installed.
2.  **Build:**
    ```bash
    swift build
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
swift run mcp-stdio-swift
```

## Swift MCP Developer Resources

*   **MCP Swift SDK (GitHub):** [https://github.com/modelcontextprotocol/swift-sdk](https://github.com/modelcontextprotocol/swift-sdk)
*   **MCP Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)

## Legacy/mismatched files
*   `Dockerfile`: Needs update for Swift.
*   `cloudbuild.yaml`: Needs update for Swift.

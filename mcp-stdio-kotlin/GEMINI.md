# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** implementation. It leverages the official `io.modelcontextprotocol:kotlin-sdk` to expose server capabilities to MCP clients. The server is currently configured to run over **stdio** transport and provides a set of tools for interaction.

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **MCP SDK:** `io.modelcontextprotocol:kotlin-sdk-jvm:0.8.1`
*   **Build Tool:** Gradle 9.2.1 (Kotlin DSL)
*   **JDK:** Java 25
*   **Transport:** Standard Input/Output (Stdio)
*   **Linter/Formatter:** Ktlint 12.1.2

## Server Capabilities

The server is initialized with the following capabilities:
*   **Tools:** Enabled (with `listChanged` notification support).

### Implemented Tools
*   `greet`: A simple tool that takes an optional `param` (string) and returns a greeting message "Hello, {param}!". Defaults to "Hello, World!" if param is omitted.

## Project Structure

*   `src/main/kotlin/com/example/mcp/server/`: Source code package.
    *   `Main.kt`: Application entry point. Sets up the `Server`, `StdioServerTransport`, and registers tools.
    *   `GreetingTool.kt`: Implementation of the `greet` tool, including its schema and handler logic.
*   `src/test/kotlin/`: Unit tests.
*   `build.gradle.kts`: Gradle build script with dependencies and plugin configuration.
*   `test_mcp.py`: A Python script for integration testing the compiled server binary via stdio.

## Development Setup

1.  **Prerequisites:**
    - Java JDK 25
    - Make (Optional)

2.  **Build:**
    ```bash
    make build
    # or
    ./gradlew build
    ```

3.  **Lint & Format:**
    ```bash
    make lint
    make format
    ```

## Running the Server

To run the server in stdio mode (usually called by an MCP client):

```bash
make run
# or
./gradlew run
```

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)

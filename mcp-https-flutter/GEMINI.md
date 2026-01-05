# Gemini Context: mcp_https_flutter

## Project Overview
This project is a Dart command-line application implementing a **Model Context Protocol (MCP)** server using the `mcp_dart` package. It communicates via HTTP (SSE for server-to-client, POST for client-to-server).

## Tech Stack
-   **Language**: Dart (SDK >= 3.10.4)
-   **Key Dependencies**:
    -   `mcp_dart`: For MCP server implementation.
    -   `path`: For file path manipulation.
-   **Dev Dependencies**:
    -   `lints`: For static analysis and enforcing Dart best practices.
    -   `test`: For unit testing.

## Project Structure
-   `bin/mcp_https_flutter.dart`: The entry point of the application. Sets up the MCP server with HTTP transport.
-   `lib/`: Contains the core logic, including tool definitions and resource handlers.
    -   `lib/tools.dart`: Definitions of MCP tools exposed by this server.
-   `test/`: Unit tests.

## Development Guidelines

### Coding Style
-   Strictly adhere to the **Dart Style Guide**.
-   Ensure all code passes the configured `lints` (check `analysis_options.yaml`).
-   Use strong typing; avoid `dynamic` unless absolutely necessary.
-   Prefer `final` for variables and fields that do not change.
-   Use `async`/`await` for asynchronous operations.

### MCP Implementation
-   **Tools**: Define tools using the `mcp_dart` primitives. Ensure clear descriptions and parameter schemas.
-   **Resources**: If implementing resources, ensure efficient reading and proper URI handling.
-   **Error Handling**: specific to MCP, return meaningful error codes/messages to the client.
-   **HTTP Transport**: The server uses SSE (Server-Sent Events) for the downstream and HTTP POST for the upstream.

### Testing
-   Write tests for all new tools and logic in `test/`.
-   Run tests using `dart test`.

## Commands
-   **Run**: `dart run` (or `dart bin/mcp_https_flutter.dart`)
-   **Test**: `dart test`
-   **Analyze**: `dart analyze`
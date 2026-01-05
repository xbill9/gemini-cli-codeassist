# Gemini Context: mcp_stdio_flutter

## Project Overview
This project is a Dart command-line application implementing a **Model Context Protocol (MCP)** server using the `mcp_dart` package. It communicates via standard input/output (stdio).

## Tech Stack
-   **Language**: Dart (SDK >= 3.10.4)
-   **Key Dependencies**:
    -   `mcp_dart`: For MCP server implementation.
    -   `path`: For file path manipulation.
-   **Dev Dependencies**:
    -   `lints`: For static analysis and enforcing Dart best practices.
    -   `test`: For unit testing.

## Project Structure
-   `bin/mcp_stdio_flutter.dart`: The entry point of the application. Sets up the MCP server and stdio connection.
-   `lib/`: Contains the core logic, including tool definitions and resource handlers.
    -   `lib/mcp_stdio_flutter.dart`: Definitions of MCP tools exposed by this server.
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
-   **Stdio**: The server relies on reading from stdin and writing to stdout. Avoid using `print` for logging/debug info as it corrupts the protocol stream. Use `stderr` or MCP logging facilities instead.

### Testing
-   Write tests for all new tools and logic in `test/`.
-   Run tests using `dart test`.

## Commands
-   **Run**: `dart run` (or `dart bin/mcp_stdio_flutter.dart`)
-   **Test**: `dart test`
-   **Analyze**: `dart analyze`

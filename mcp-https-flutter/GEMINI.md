# Gemini Context: mcp_https_flutter

## Project Overview
This project is a Dart command-line application implementing a **Model Context Protocol (MCP)** server using the `mcp_dart` package. It uses `StreamableMcpServer` to provide an HTTP-based transport (SSE for events, POST for requests).

## Tech Stack
-   **Language**: Dart (SDK >= 3.10.4)
-   **Key Dependencies**:
    -   `mcp_dart`: For MCP server implementation (v1.2.0+).
    -   `path`: For file path manipulation.
    -   `args`: For parsing command-line arguments.
    -   `logging`: For structured logging.
-   **Dev Dependencies**:
    -   `lints`: For static analysis.
    -   `test`: For unit testing.

## Project Structure
-   `bin/mcp_https_flutter.dart`: The entry point. Configures logging, parses arguments, and starts the `StreamableMcpServer`.
-   `lib/mcp_https_flutter.dart`: Contains the core logic, including the `greet` tool handler (`greetHandler`) and server creation factory.
-   `test/`: Unit tests.

## Development Guidelines

### Coding Style
-   Strictly adhere to the **Dart Style Guide**.
-   Ensure all code passes the configured `lints`.
-   Use strong typing.
-   Use `async`/`await` for asynchronous operations.
-   Use `logging` package for all output (mapped to `stderr` with JSON formatting).

### MCP Implementation
-   **Server**: Uses `StreamableMcpServer` which handles the HTTP/SSE transport details.
-   **Tools**: Defined in `createServer()` within `bin/mcp_https_flutter.dart`.
    -   `greet`: A simple tool that echoes back a parameter.
-   **Handlers**: Implementation logic is separated into functions (e.g., `greetHandler` in `lib/mcp_https_flutter.dart`).
-   **Logging**: The server uses structured JSON logging to `stderr`.

### Testing
-   Write tests in `test/`.
-   Run tests using `dart test`.

## Commands
-   **Run**: `dart run` or `dart bin/mcp_https_flutter.dart`
    -   Supports arguments: `--port`, `--host`, `--path`.
-   **Test**: `dart test`
-   **Analyze**: `dart analyze`

# Dart MCP Server

This is a **Dart-based Model Context Protocol (MCP) server** using the `mcp_dart` package. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** Dart
*   **SDK:** `mcp_dart` (Model Context Protocol SDK for Dart)
*   **Dependency Management:** `pub` / `pubspec.yaml`

## Prerequisites

*   Dart SDK (version 3.10.0 or later)

## Setup

1.  **Get dependencies:**
    ```bash
    dart pub get
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
dart run bin/firestore_stdio_flutter.dart
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Tools

*   `greet`: Takes a `param` (string) and returns it.
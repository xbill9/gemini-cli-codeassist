# mcp-https-flutter

A Model Context Protocol (MCP) server implemented in Dart, supporting both Stdio and Streamable HTTP transports.

## Usage

### Stdio Transport

```bash
dart run bin/mcp_https_flutter.dart --transport stdio
```

### Streamable HTTP Transport

```bash
dart run bin/mcp_https_flutter.dart --transport http --port 8080 --host localhost --path /mcp
```


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
dart run bin/mcp_https_flutter.dart
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Tools

*   `greet`: Takes a `param` (string) and returns it.
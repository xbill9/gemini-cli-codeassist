# MCP Stdio Swift Server

A simple Model Context Protocol (MCP) server implemented in Swift. This server communicates over `stdio` and serves as a "Hello World" example for Swift-based MCP integrations.

## Overview

This project provides a basic MCP server named `hello-world-server` that exposes a single tool: `greet`. It uses the `swift-log` library for logging to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

- **Swift 6.0+** (or compatible Swift toolchain)
- **Linux / macOS**

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-swift
    ```

2.  **Build the project:**
    ```bash
    make build
    # Or manually:
    swift build -c release
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
.build/release/mcp-stdio-swift
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this:

```json
{
  "mcpServers": {
    "swift-hello-world": {
      "command": "/path/to/mcp-stdio-swift/.build/release/mcp-stdio-swift",
      "args": []
    }
  }
}
```

*Note: Ensure you have built the project with `swift build -c release` first.*

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param`.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Install dependencies:** `make install`
- **Run the server (debug):** `make run`
- **Build the server:** `make build`
- **Test:** `make test`
- **Clean artifacts:** `make clean`

## Project Structure

- `Package.swift`: Swift package definition and dependencies.
- `Sources/mcp-stdio-swift/main.swift`: Entry point defining the server and tools.
- `Makefile`: Commands for build, test, and maintenance.
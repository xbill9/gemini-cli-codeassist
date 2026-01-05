# MCP Stdio Swift Server

A simple Model Context Protocol (MCP) server implemented in Swift. This server communicates over `stdio` and serves as a "Hello World" example for Swift-based MCP integrations.

## Overview

This project provides a basic MCP server named `hello-world-server` that exposes a single tool: `greet`. 

**Key Features:**
*   **Transport:** Uses standard input/output (`stdio`) for MCP communication.
*   **Concurrency:** Built on Swift's structured concurrency and `ServiceLifecycle` for graceful shutdown.
*   **Logging:** Uses `swift-log` directed to `stderr` to ensure the `stdout` channel remains clean for protocol messages.

## Prerequisites

- **Swift 6.0+** (or compatible Swift toolchain)
- **Linux** or **macOS**

## Getting Started

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-swift
    ```

2.  **Build the project:**
    
    For development (debug build):
    ```bash
    make build
    # Executable will be at: .build/debug/mcp-stdio-swift
    ```

    For production (release build):
    ```bash
    swift build -c release
    # Executable will be at: .build/release/mcp-stdio-swift
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually for testing (starts listening on stdio):
```bash
.build/debug/mcp-stdio-swift
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), use the **absolute path** to the executable.

**Example (using release build):**

```json
{
  "mcpServers": {
    "swift-hello-world": {
      "command": "/absolute/path/to/mcp-stdio-swift/.build/release/mcp-stdio-swift",
      "args": []
    }
  }
}
```

*Note: Run `swift build -c release` before configuring the client.*

## Tools

### `greet`
- **Description:** Get a greeting from the local stdio server.
- **Parameters:**
    - `param` (string, required): The name or parameter to greet.
- **Returns:** The string passed in `param`.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Install dependencies:** `make install`
- **Run the server (debug):** `make run`
- **Build the server (debug):** `make build`
- **Test:** `make test`
- **Clean artifacts:** `make clean`

## Project Structure

- `Package.swift`: Swift package definition, dependencies, and targets.
- `Sources/mcp-stdio-swift/main.swift`: Entry point defining the server, `ServiceLifecycle` setup, and tool implementations.
- `Sources/mcp-stdio-swift/Handlers.swift`: Tool implementations (`greet`).
- `Sources/mcp-stdio-swift/MCPService.swift`: Service lifecycle integration.
- `Makefile`: Shortcuts for build, test, and maintenance.

# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Zig-based Model Context Protocol (MCP) server** named `mcp-https-zig`. It uses the `mcp` Zig library (managed via Zig Package Manager) to implement the protocol.
It uses a custom **HTTP Transport** listening on port 8080.

## Key Technologies

*   **Language:** Zig (v0.15.2+)
*   **Library:** `mcp` (Zig library, fetched via `build.zig.zon`)
*   **Build System:** `zig build`
*   **Automation:** `Makefile` for common tasks.
*   **Communication:** HTTP (port 8080) for MCP messages; `stderr` for structured JSON logging.
*   **Testing:** Zig built-in testing framework (`zig build test`) and Python integration script (`test_server.py`).

mcp zig package: https://github.com/muhammad-fiaz/mcp.zig

## Project Structure

*   `src/main.zig`: The entry point. Initializes the server, defines the `greet` tool, and implements the `HttpServerTransport` struct for handling HTTP connections.
*   `build.zig`: Build configuration for compiling the Zig executable (`mcp-https-zig`) and linking dependencies.
*   `build.zig.zon`: Package manifest declaring the `mcp` dependency.
*   `Makefile`: Shortcuts for build, test, clean, format, and lint commands.
*   `test_server.py`: Python script for integration testing the running server.
*   `test_net.zig`: Additional networking tests (if applicable).
*   `check_io.zig`: IO checking utility (if applicable).

## Implemented Tools

### `greet`
*   **Description:** Get a greeting from a local http server.
*   **Parameters:**
    *   `param` (string): Greeting parameter.
*   **Implementation:** `greetHandler` function in `src/main.zig`.

## API Usage (mcp library)

The project uses the `mcp` library imported via `@import("mcp")`. Key operations include:

*   **Server Initialization:** `mcp.Server.init(...)`
*   **Tool Definition:** `server.addTool(...)`
*   **Transport:** Custom `HttpServerTransport` struct that maps HTTP requests/responses to MCP `Transport` vtable (`send`, `receive`, `close`).

## Build & Development

The `build.zig` supports the following steps:

*   **Compile:** Compiles `src/main.zig` and links `mcp` dependency.
*   **Test:** Runs unit tests defined in Zig sources.

## Running the Server

```bash
./zig-out/bin/mcp-https-zig
```
or
```bash
make build && ./server
```

*   **Input:** HTTP POST requests containing JSON-RPC 2.0 messages.
*   **Output:** HTTP Responses containing JSON-RPC 2.0 messages.
*   **Logs:** JSON formatted logs via `stderr`.
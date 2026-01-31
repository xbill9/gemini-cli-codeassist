# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Zig-based Model Context Protocol (MCP) server** named `mcp-https-zig`. It uses the `mcp` Zig library (managed via Zig Package Manager) to implement the protocol. It exposes a single tool (`greet`) over HTTP and logs structured JSON to stderr.

## Key Technologies

*   **Language:** Zig (v0.15.2+)
*   **Library:** `mcp` (Zig library, fetched via `build.zig.zon`)
*   **Build System:** `zig build`
*   **Automation:** `Makefile` for common tasks.
*   **Communication:** HTTP (port 8080) for MCP; `stderr` for logging.
*   **Testing:** Zig built-in testing framework (`zig build test`).

mcp zig package: https://github.com/muhammad-fiaz/mcp.zig

## Project Structure

*   `src/main.zig`: The entry point. Initializes the server, defines the `greet` tool, and handles the event loop.
*   `build.zig`: Build configuration for compiling the Zig executable and dependencies.
*   `build.zig.zon`: Package manifest declaring the `mcp` dependency.
*   `Makefile`: Shortcuts for build, test, clean, format, and lint commands.
*   `test_server.py`: Python script for integration testing the compiled server.

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

*   **Input:** JSON-RPC 2.0 messages via `stdin`.
*   **Output:** JSON-RPC 2.0 messages via `stdout`.
*   **Logs:** JSON formatted logs via `stderr`.

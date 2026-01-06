# MCP HTTPS Kotlin Server

A simple Model Context Protocol (MCP) server implemented in Kotlin using the official `kotlin-sdk`. This server is designed to communicate over **HTTPS (SSE)** and serves as a foundational "Hello World" example for Kotlin-based MCP integrations.

## Overview

This project provides a basic MCP server named `mcp-https-server` that exposes a single tool: `greet`. It runs a Ktor web server and communicates via Server-Sent Events (SSE).

## Prerequisites

- **Java JDK 25** (Required for Gradle 9.2.1+ and Kotlin 2.3.0+ compilation)
- **Gradle 9.2.1+** (Wrapper included)

## Building the Project

1.  **Build the project:**
    ```bash
    make build
    # Or manually:
    ./gradlew build
    ```
    This will compile the Kotlin code and produce a fat JAR (distribution) in `build/libs/`.

## Running the Server

To run the server manually:
```bash
make run
# Or manually:
./gradlew run
```
The server will start on port `8080` (default) or the port specified by the `PORT` environment variable.

### Endpoints
- **SSE Endpoint:** `http://localhost:8080/sse`
- **POST Endpoint:** `http://localhost:8080/messages`

### Configuration for MCP Clients

To connect an MCP client (like an IDE extension or Claude Desktop if supported) to this server, you typically configure it to point to the SSE URL.

**Example Configuration (Generic):**

```json
{
  "mcpServers": {
    "kotlin-https-server": {
      "url": "http://localhost:8080/sse"
    }
  }
}
```

*Note: Since this is an HTTP server, you must run the server (`make run`) separately before the client attempts to connect, unless your client supports launching HTTP servers automatically.*

## Tools

### `greet`
- **Description:** Get a greeting from a local HTTPS server.
- **Parameters:**
    - `param` (string): The name to greet.
- **Returns:** A string greeting.

## Project Structure

- `src/main/kotlin/.../Main.kt`: Entry point defining the Ktor server, SSE configuration, and MCP tools.
- `build.gradle.kts`: Gradle build configuration (Kotlin DSL).
- `Makefile`: Commands for build, test, and maintenance.
# MCP HTTPS Kotlin Server

A robust Model Context Protocol (MCP) server implemented in Kotlin using the official `kotlin-sdk` and **Ktor**. This server communicates over **HTTPS (SSE)** and serves as a reference implementation for Kotlin-based MCP integrations.

## Overview

This project provides an MCP server named `mcp-https-server` that exposes a sample tool (`greet`). It leverages Ktor for the web layer, handling Server-Sent Events (SSE) and message processing efficiently.

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **SDK:** `io.modelcontextprotocol:kotlin-sdk`
*   **Web Framework:** Ktor 3.0.0 (Netty, SSE)
*   **Build Tool:** Gradle 9.2.1
*   **JDK:** Java 25

## Prerequisites

- **Java JDK 25** (Required for Gradle 9.2.1+ and Kotlin 2.3.0+)
- **Gradle 9.2.1+** (Wrapper included)

## Building the Project

**Build the project:**
```bash
make build
# Or manually:
./gradlew build
```
This will compile the Kotlin code and produce a distribution in `build/libs/`.

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

## Development

### Project Structure

*   `src/main/kotlin/.../Main.kt`: Application entry point. Configures Ktor, SSE, and the MCP Server.
*   `src/main/kotlin/.../Tools.kt`: Tool definitions and registration logic.
*   `src/main/kotlin/.../Config.kt`: Centralized configuration (routes, constants).
*   `src/main/kotlin/.../SessionService.kt`: Manages MCP sessions over SSE.
*   `gradle/libs.versions.toml`: Dependency version catalog.

### Testing

Run the test suite:
```bash
make test
# Or manually:
./gradlew test
```

### Linting & Formatting

This project uses `ktlint` to enforce coding conventions.

*   **Check styles:** `./gradlew ktlintCheck`
*   **Fix styles:** `./gradlew ktlintFormat`

## Configuration for MCP Clients

To connect an MCP client (like an IDE extension or Claude Desktop) to this server, configure it to point to the SSE URL.

**Example Configuration:**

```json
{
  "mcpServers": {
    "kotlin-https-server": {
      "url": "http://localhost:8080/sse"
    }
  }
}
```

*Note: Since this is an HTTP server, you must ensure the server is running (`make run`) before the client attempts to connect.*

## Tools

### `greet`
- **Description:** Get a greeting from a local HTTPS server.
- **Parameters:**
    - `param` (string): The name to greet.
- **Returns:** A string greeting.

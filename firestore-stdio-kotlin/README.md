# MCP Stdio Kotlin Server

A simple Model Context Protocol (MCP) server implemented in Kotlin using the official `kotlin-sdk`. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for Kotlin-based MCP integrations.

## Overview

This project provides a basic MCP server named `hello-world-server` that exposes a single tool: `greet`. It communicates via standard input/output (stdio).

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

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually:
```bash
make run
# Or manually:
./gradlew run
```

### Configuration for MCP Clients

If you are adding this to an MCP client config, the configuration would look something like this:

```json
{
  "mcpServers": {
    "kotlin-hello-world": {
      "command": "java",
      "args": ["-jar", "/path/to/mcp-stdio-kotlin/build/libs/mcp-stdio-kotlin-1.0-SNAPSHOT.jar"]
    }
  }
}
```

*Note: Ensure the absolute path is correct. The standard build produces `mcp-stdio-kotlin-1.0-SNAPSHOT.jar`.*

## Tools

### `greet`
- **Description:** Get a greeting from a local stdio server.
- **Parameters:**
    - `param` (string): The name to greet.
- **Returns:** A string greeting.

## Project Structure

- `src/main/kotlin/.../Main.kt`: Entry point defining the server and tools.
- `build.gradle.kts`: Gradle build configuration (Kotlin DSL).
- `Makefile`: Commands for build, test, and maintenance.
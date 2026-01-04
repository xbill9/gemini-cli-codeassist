# MCP Stdio Kotlin Server

A Model Context Protocol (MCP) server implemented in Kotlin using the official [MCP Kotlin SDK](https://github.com/modelcontextprotocol/kotlin-sdk). This server communicates over standard input/output (stdio) and serves as a robust example for building Kotlin-based MCP integrations.

## Overview

This project implements a server named `hello-world-server` that exposes tools to MCP clients. It demonstrates:
- Initializing an MCP server with the `kotlin-sdk`.
- Defining and registering tools with JSON schemas.
- Handling tool calls via `StdioServerTransport`.
- Coroutine-based session management.

## Technical Stack

- **Language:** Kotlin 2.3.0
- **MCP SDK:** `io.modelcontextprotocol:kotlin-sdk-jvm:0.8.1`
- **JDK:** Java 25
- **Build System:** Gradle 9.2.1

## Prerequisites

- **Java JDK 25** (Required for the latest Kotlin and Gradle features used in this project)
- **Gradle 9.2.1+** (The project includes a Gradle wrapper)

## Getting Started

### Building the Project

Compile the project and prepare the distribution:
```bash
make build
# or
./gradlew build installDist
```
The executable scripts will be generated in `build/install/mcp-stdio-kotlin/bin/`.

### Running the Server Locally

You can run the server directly using Gradle (mainly for testing):
```bash
make run
# or
./gradlew run
```

### Configuration for MCP Clients

To use this server with an MCP client (such as Claude Desktop or a Gemini-integrated IDE), use the absolute path to the generated script or the JAR.

**Using the generated binary (recommended):**

```json
{
  "mcpServers": {
    "kotlin-mcp-server": {
      "command": "/path/to/mcp-stdio-kotlin/build/install/mcp-stdio-kotlin/bin/mcp-stdio-kotlin",
      "args": []
    }
  }
}
```

## Available Tools

### `greet`
- **Description:** Returns a friendly greeting message.
- **Input Schema:**
  - `param` (string, required): The name of the person or entity to greet.
- **Output:** A `TextContent` object containing "Hello, {param}!".

## Project Structure

- `src/main/kotlin/com/example/mcp/server/Main.kt`: Server initialization and transport setup.
- `src/main/kotlin/com/example/mcp/server/GreetingTool.kt`: Tool definition and execution logic.
- `src/test/kotlin/`: Unit tests using `kotlin.test`.
- `test_mcp.py`: Integration test script to verify stdio communication.
- `build.gradle.kts`: Project dependencies and build configuration.

## Testing

Run unit tests:
```bash
make test
# or
./gradlew test
```

Run integration tests (requires building first):
```bash
make build
python3 test_mcp.py
```

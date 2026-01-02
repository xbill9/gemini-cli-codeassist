# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** using the official `kotlin-sdk`. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients.

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **SDK:** `io.modelcontextprotocol:kotlin-sdk`
*   **Build Tool:** Gradle 9.2.1 (Kotlin DSL)
*   **JDK:** Java 25
*   **MCP SDK:** https://github.com/modelcontextprotocol/kotlin-sdk

## Project Structure

*   `src/main/kotlin/.../Main.kt`: The entry point. Initializes the server and defines tools.
*   `build.gradle.kts`: Gradle build configuration.
*   `settings.gradle.kts`: Gradle settings.
*   `Makefile`: Development shortcuts.

## Development Setup

1.  **Prerequisites:**
    - Java JDK 25

2.  **Build:**
    ```bash
    make build
    # or
    ./gradlew build
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
make run
# or
./gradlew run
```

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)

## Legacy/mismatched files
*   `Dockerfile`: Needs update for Kotlin.
*   `cloudbuild.yaml`: Needs update for Kotlin.

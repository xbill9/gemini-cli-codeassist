# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** using the official `kotlin-sdk` and **Ktor**. It communicates over **HTTPS (SSE)** and exposes tools (like `greet`).

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **SDK:** `io.modelcontextprotocol:kotlin-sdk` (v0.8.1)
*   **Web Framework:** Ktor 3.0.0
*   **Build Tool:** Gradle 9.2.1 (Kotlin DSL)
*   **JDK:** Java 25

## Project Structure

*   `src/main/kotlin/.../Main.kt`: The entry point. Configures Ktor, SSE transport, and MCP tools.
*   `gradle/libs.versions.toml`: Gradle version catalog for dependency management.
*   `build.gradle.kts`: Gradle build configuration using the version catalog.
*   `settings.gradle.kts`: Gradle settings.
*   `Makefile`: Development shortcuts.
*   `Dockerfile`: Container definition for the server.
*   `cloudbuild.yaml`: Google Cloud Build configuration.

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

The server runs on port `8080` (default) using the `HTTPS` (SSE) transport.

```bash
make run
# or
./gradlew run
```

### Endpoints
- **SSE:** `http://localhost:8080/sse`
- **POST:** `http://localhost:8080/messages`

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)

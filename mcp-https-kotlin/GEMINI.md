# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** using the official `kotlin-sdk` and **Ktor**. It communicates over **HTTPS (SSE)** and exposes tools (like `greet`).

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **SDK:** `io.modelcontextprotocol:kotlin-sdk` (v0.8.1)
*   **Web Framework:** Ktor 3.0.0 (Netty, SSE, ContentNegotiation, CORS)
*   **Build Tool:** Gradle 9.2.1 (Kotlin DSL)
*   **JDK:** Java 25
*   **Style Guide:** [Kotlin Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html) (Enforced by `ktlint`)

## Project Structure

### Source Code (`src/main/kotlin/com/example/mcp/server/`)

*   **`Main.kt`**: Application entry point. Configures the Ktor server, installs plugins (SSE, Serialization, CORS), initializes the MCP `Server` instance, and sets up routing.
*   **`Tools.kt`**: Contains tool definitions and registration logic.
    *   **Best Practice:** Keep tool logic modular. Use `server.addTool` to register capabilities. Separating the "business logic" (e.g., `formatGreeting`) from the registration closure allows for easier unit testing.
*   **`SessionService.kt`**: Manages the lifecycle of MCP sessions over SSE. It handles the specific requirements of bridging Ktor's `SSESession` with the MCP SDK's input/output channels.
*   **`Config.kt`**: Centralized configuration constants (Server Name, Version, Ports, Routes, Tool Names).
    *   **Best Practice:** Avoid hardcoding strings in logic files; use this object for maintainability.
*   **`Logger.kt`**: Logging utilities (wrapper around SLF4J).

### Build & Config

*   **`gradle/libs.versions.toml`**: Gradle Version Catalog.
    *   **Best Practice:** All dependencies and plugin versions are defined here. Do not add versions directly in `build.gradle.kts`.
*   **`build.gradle.kts`**: Main build script. Configures the Kotlin JVM plugin, Ktor dependencies, and testing tasks.
*   **`settings.gradle.kts`**: Project settings.
*   **`Makefile`**: Development shortcuts (`make build`, `make run`, `make test`).
*   **`Dockerfile`**: Container definition for production deployment.

## Development Setup

1.  **Prerequisites:**
    - Java JDK 25

2.  **Build:**
    ```bash
    make build
    # or
    ./gradlew build
    ```

3.  **Run Tests:**
    ```bash
    make test
    # or
    ./gradlew test
    ```

## Running the Server

The server runs on port `8080` (default) using the `HTTPS` (SSE) transport.

```bash
make run
# or
./gradlew run
```

### Endpoints (Defined in `Config.kt`)
-   **SSE:** `http://localhost:8080/sse` (Handled by `SessionService`)
-   **POST:** `http://localhost:8080/messages` (Handled by `SessionService`)

## Coding Conventions & Best Practices

*   **Tool Implementation:**
    *   Define tools in `Tools.kt` or dedicated service classes for larger applications.
    *   Use `ToolSchema` with `kotlinx.serialization` helpers (`buildJsonObject`) to define input schemas clearly.
    *   Always extract logic into testable functions (like `formatGreeting`) instead of putting everything inside the `addTool` lambda.
*   **Configuration:**
    *   Use the `Config` object for all constants.
    *   Read sensitive or environment-specific values (like ports) from environment variables in `Main.kt`, falling back to `Config` defaults.
*   **Concurrency:**
    *   The project uses Kotlin Coroutines. Ensure blocking operations are dispatched to appropriate dispatchers if necessary, though Ktor and the MCP SDK are designed to be asynchronous.
*   **Formatting:**
    *   Run `./gradlew ktlintCheck` to verify style.
    *   Run `./gradlew ktlintFormat` to fix style issues automatically.

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)
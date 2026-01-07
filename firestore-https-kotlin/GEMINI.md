# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** using the official `kotlin-sdk` and **Google Cloud Firestore**. It exposes tools over streaming HTTP (SSE) for integration with MCP clients to manage a product inventory.

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **SDK:** `io.modelcontextprotocol:kotlin-sdk`
*   **Database:** Google Cloud Firestore
*   **Server:** Ktor (Netty, SSE)
*   **Build Tool:** Gradle 9.2.1 (Kotlin DSL)
*   **JDK:** Java 25

## MCP Tools

The server exposes the following tools:

*   `greet`: A simple greeting tool.
*   `get_products`: Retrieves all products from the Firestore inventory.
*   `get_product_by_id`: Retrieves a single product by its ID.
*   `search`: Searches for products by name.
*   `seed`: Seeds the database with initial product data.
*   `reset`: Deletes all products from the database.
*   `get_root`: Returns a welcome message.

## Project Structure

*   `src/main/kotlin/.../Main.kt`: The main entry point containing the MCP server setup, Firestore logic, and tool definitions.
*   `gradle/libs.versions.toml`: Gradle version catalog for dependency management.
*   `build.gradle.kts`: Gradle build configuration.
*   `settings.gradle.kts`: Gradle settings.
*   `Makefile`: Development shortcuts.
*   `Dockerfile`: Container configuration for the application.
*   `cloudbuild.yaml`: Google Cloud Build configuration.

## Development Setup

1.  **Prerequisites:**
    - Java JDK 25
    - Google Cloud Project with Firestore enabled (for local development, ensure `GOOGLE_CLOUD_PROJECT` is set and authentication is configured).

2.  **Build:**
    ```bash
    make build
    # or
    ./gradlew build
    ```

## Running the Server

The server runs on port 8080 by default.

```bash
make run
# or
./gradlew run
```

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)
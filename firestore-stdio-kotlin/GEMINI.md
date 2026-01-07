# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** implementation that manages an inventory in **Google Cloud Firestore**. It leverages the official `io.modelcontextprotocol:kotlin-sdk` to expose server capabilities to MCP clients. The server is configured to run over **stdio** transport.

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **MCP SDK:** `io.modelcontextprotocol:kotlin-sdk-jvm:0.8.1`
*   **Build Tool:** Gradle 9.2.1 (Kotlin DSL)
*   **JDK:** Java 25
*   **Database:** Google Cloud Firestore
*   **Transport:** Standard Input/Output (Stdio)

## Server Capabilities

The server is initialized with the following capabilities:
*   **Tools:** Enabled (with `listChanged` notification support).

### Implemented Tools
*   `get_products`: Returns a JSON list of all products in the inventory.
*   `get_product_by_id`: Returns a single product by its Firestore ID.
*   `seed`: Seeds the database with sample inventory items.
*   `reset`: Deletes all items from the inventory collection.
*   `check_db`: Verifies if the Firestore client is active.
*   `get_root`: Returns a greeting message from the Cymbal Superstore Inventory API.

## Project Structure

*   `src/main/kotlin/com/example/mcp/server/`: Source code package.
    *   `Main.kt`: Application entry point. Sets up the `Server`, `StdioServerTransport`, and registers the tools.
    *   `FirestoreService.kt`: Contains the Firestore interaction logic, data models (`Product`), and methods for tool handlers.
*   `build.gradle.kts`: Gradle build script with dependencies and plugin configuration.
*   `Makefile`: Build and execution shortcuts.
*   `test_mcp.py`: A Python script for integration testing the server via stdio.

## Development Setup

1.  **Prerequisites:**
    - Java JDK 25
    - Google Cloud project with Firestore enabled.
    - Application Default Credentials (`gcloud auth application-default login`).

2.  **Build:**
    ```bash
    make build
    # or
    ./gradlew build installDist
    ```

3.  **Run:**
    ```bash
    make run
    # or
    ./gradlew run
    ```

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)

## Best Practices

### Kotlin & Code Quality
*   **Idiomatic Kotlin:** Leverage data classes for models, extension functions for utility logic, and Kotlin's null safety features.
*   **Coroutines:** Use Kotlin Coroutines for asynchronous operations, especially I/O bound tasks like database calls. Ensure proper `CoroutineScope` management.
*   **Linting:** Maintain code style using `ktlint`. Run `./gradlew ktlintCheck` before commits.

### Firestore & Google Cloud
*   **Authentication:** Rely on Application Default Credentials (ADC) for seamless local and cloud authentication. Avoid hardcoding service account keys.
*   **Data Modeling:** Use simple, flat data structures for Firestore documents where possible. Map Firestore documents directly to Kotlin data classes.
*   **Error Handling:** Wrap Firestore operations in `try-catch` blocks to handle exceptions (e.g., connection issues, permission errors) and return user-friendly messages.

### MCP Server Development
*   **Tool Definitions:** Clearly document input schemas and descriptions for all tools. This helps the LLM understand how to use them.
*   **Stdio Transport:** Ensure no stray `System.out.println` calls pollute the standard output, as this is used for the MCP protocol transport. Use a logging framework (SLF4J/Logback) writing to stderr or a file.
*   **State Changes:** When tools modify data (like `seed` or `reset`), consider sending notifications if clients subscribe to resource updates (if implemented).
*   **Atomic Operations:** Design tools to be atomic. If a tool fails, it should ideally leave the system in a consistent state.

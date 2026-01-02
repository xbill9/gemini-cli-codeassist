# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Kotlin-based Model Context Protocol (MCP) server** using the official `kotlin-sdk`. It implements an **Inventory Management Server** that interacts with **Google Cloud Firestore**. It exposes tools to manage products via standard input/output (stdio).

## Key Technologies

*   **Language:** Kotlin 2.3.0
*   **SDK:** `io.modelcontextprotocol:kotlin-sdk`
*   **Database:** Google Cloud Firestore (`com.google.cloud:google-cloud-firestore`)
*   **Build Tool:** Gradle 8.12 (Kotlin DSL)
*   **JDK:** Java 25

## Project Structure

*   `src/main/kotlin/com/example/mcp/server/Main.kt`: Entry point. Initializes the MCP server, defines tools, and sets up the stdio transport.
*   `src/main/kotlin/com/example/mcp/server/FirestoreService.kt`: Handles Firestore interactions, data modeling (Product), and business logic.
*   `build.gradle.kts`: Gradle build configuration.
*   `Makefile`: Development shortcuts for building and running.

## Development Setup

1.  **Prerequisites:**
    - Java JDK 25
    - Google Cloud Credentials (configured via `GOOGLE_APPLICATION_CREDENTIALS` environment variable or `gcloud auth application-default login`)

2.  **Build:**
    ```bash
    make build
    # or
    ./gradlew build
    ```

## Running the Server

The server runs using the `stdio` transport.

```bash
make run
# or
./gradlew run
```

## Tools Summary

*   `get_products`: List all products in the inventory.
*   `get_product_by_id`: Retrieve a specific product by its Firestore ID.
*   `seed`: Populate the database with sample inventory data.
*   `reset`: Remove all products from the inventory.
*   `check_db`: Verify Firestore connectivity.
*   `get_root`: Returns a basic greeting.

## Resources

*   **MCP Kotlin SDK (GitHub):** [https://github.com/modelcontextprotocol/kotlin-sdk](https://github.com/modelcontextprotocol/kotlin-sdk)

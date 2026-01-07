# Cymbal Superstore Inventory MCP Server (Kotlin)

A Model Context Protocol (MCP) server implemented in Kotlin using the official `kotlin-sdk` and Google Cloud Firestore. This server exposes tools to manage a product inventory over HTTP using Server-Sent Events (SSE).

## Overview

This project provides an MCP server named `cymbal-inventory` that allows MCP clients to interact with a product database stored in Firestore. It supports listing products, searching, and managing the database state.

## Prerequisites

- **Java JDK 25** (Required for Gradle 9.2.1+ and Kotlin 2.3.0+)
- **Google Cloud Project** with Firestore enabled.
- **Application Default Credentials** or `GOOGLE_CLOUD_PROJECT` environment variable set for Firestore access.

## Building the Project

1.  **Build the project:**
    ```bash
    make build
    # Or manually:
    ./gradlew build
    ```

## Running the Server

To run the server locally:
```bash
make run
# Or manually:
./gradlew run
```
By default, the server runs on port `8080`.

## Tools

### `get_products`
- **Description:** Get a list of all products from the inventory database.
- **Returns:** A JSON list of products.

### `get_product_by_id`
- **Description:** Get a single product from the inventory database by its ID.
- **Parameters:**
    - `id` (string): The ID of the product.

### `search`
- **Description:** Search for products in the inventory database by name.
- **Parameters:**
    - `query` (string): The search query to filter products by name.

### `greet`
- **Description:** A simple greeting tool.
- **Parameters:**
    - `name` (string, optional): Name to greet.

### `seed`
- **Description:** Seed the inventory database with initial products.

### `reset`
- **Description:** Clear all products from the inventory database.

### `get_root`
- **Description:** Get a greeting from the Cymbal Superstore Inventory API.

## Project Structure

- `src/main/kotlin/com/example/mcp/server/Main.kt`: Entry point defining the server, Firestore logic, and tools.
- `build.gradle.kts`: Gradle build configuration (Kotlin DSL).
- `Makefile`: Development shortcuts (build, run, test, deploy).
- `Dockerfile` & `cloudbuild.yaml`: Configuration for containerization and Google Cloud Build.

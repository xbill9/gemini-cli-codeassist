# Firestore Inventory MCP Server

A Model Context Protocol (MCP) server implemented in Kotlin that provides an inventory management interface backed by Google Cloud Firestore.

## Overview

This project implements an MCP server named `inventory-server`. It exposes tools to query, seed, and manage a product inventory stored in Firestore. It communicates via standard input/output (stdio), making it compatible with MCP clients like Claude Desktop or Gemini-powered IDE extensions.

## Prerequisites

- **Java JDK 25**
- **Google Cloud Project** with Firestore enabled.
- **Authentication:** Ensure you have active application default credentials.
  ```bash
  gcloud auth application-default login
  ```

## Building the Project

1.  **Build the project:**
    ```bash
    make build
    # Or manually:
    ./gradlew build installDist
    ```
    This compiles the Kotlin code and installs the distribution in `build/install/firestore-stdio-kotlin/`.

## Running the Server

### Manual Execution
```bash
make run
# Or manually:
./gradlew run
```

### Configuration for MCP Clients

To use this with an MCP client, add it to your configuration file (e.g., `claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "firestore-inventory": {
      "command": "/path/to/firestore-stdio-kotlin/build/install/firestore-stdio-kotlin/bin/firestore-stdio-kotlin",
      "args": [],
      "env": {
        "GOOGLE_APPLICATION_CREDENTIALS": "/path/to/your/service-account-file.json"
      }
    }
  }
}
```

*Note: The `command` path should be absolute.*

## Tools

- **`get_products`**: Get a list of all products from the inventory database.
- **`get_product_by_id`**: Get a single product by its ID.
  - Arguments: `id` (string)
- **`seed`**: Seed the inventory database with sample products.
- **`reset`**: Clears all products from the inventory database.
- **`check_db`**: Checks if the inventory database is running.
- **`get_root`**: Get a greeting from the API.

## Project Structure

- `src/main/kotlin/.../Main.kt`: Server entry point and tool definitions.
- `src/main/kotlin/.../FirestoreService.kt`: Firestore logic and data models.
- `build.gradle.kts`: Gradle build configuration.
- `Makefile`: Commands for building and running.

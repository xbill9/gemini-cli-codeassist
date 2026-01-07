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

The server exposes the following tools:

- **`get_products`**: Get a JSON list of all products from the inventory collection.
- **`get_product_by_id`**: Get a single product from the inventory database by its ID.
  - **Arguments**:
    - `id` (string): The ID of the product to retrieve.
- **`seed`**: Seed the inventory database with a set of sample products (both old and new stock).
- **`reset`**: Clears all documents from the `inventory` collection in Firestore.
- **`check_db`**: Checks if the connection to Firestore is initialized and active.
- **`get_root`**: Get a greeting from the Cymbal Superstore Inventory API.

## Data Model

The inventory uses the following `Product` schema:

| Field | Type | Description |
|-------|------|-------------|
| `id` | String | Unique identifier (Firestore document ID) |
| `name` | String | Name of the product |
| `price` | Integer | Price of the product |
| `quantity` | Integer | Stock quantity |
| `imgfile` | String | Path to the product image |
| `timestamp` | String (ISO-8601) | Date associated with the product stock |
| `actualdateadded` | String (ISO-8601) | Date the product was added to the database |

## Project Structure

- `src/main/kotlin/com/example/mcp/server/Main.kt`: Server entry point and MCP tool registrations.
- `src/main/kotlin/com/example/mcp/server/FirestoreService.kt`: Firestore interaction logic, data models, and seeding/resetting functionality.
- `build.gradle.kts`: Gradle build configuration and dependencies.
- `Makefile`: Convenient shortcuts for building and running.

## Development

- **Language:** Kotlin 2.3.0
- **JDK:** 25
- **MCP SDK:** `io.modelcontextprotocol:kotlin-sdk-jvm:0.8.1`
- **Database:** Google Cloud Firestore
# Inventory MCP PHP Server

A Model Context Protocol (MCP) server implemented in PHP for managing a Cymbal Superstore inventory via Google Cloud Firestore.

## Overview

This project provides an MCP server named `inventory-server` that exposes several tools to interact with a Firestore-backed inventory. It uses `monolog/monolog` for structured logging to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

- **PHP 8.1+** (Tested on 8.2)
- `composer` (PHP Dependency Manager)
- Google Cloud Project with Firestore enabled.
- Proper authentication (e.g., `GOOGLE_APPLICATION_CREDENTIALS` environment variable).

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-php
    ```

2.  **Install dependencies:**
    ```bash
    make install
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
php main.php
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this:

```json
{
  "mcpServers": {
    "inventory-server": {
      "command": "php",
      "args": ["/path/to/firestore-stdio-php/main.php"]
    }
  }
}
```

*Note: Ensure the absolute path is correct and that the `php` command is in your system PATH.*

## Tools

- `get_products`: Get a list of all products from the inventory database.
- `get_product_by_id`: Get a single product from the inventory database by its ID.
- `seed`: Seed the inventory database with sample products.
- `reset`: Clears all products from the inventory database.
- `get_root`: Get a greeting from the Cymbal Superstore Inventory API.
- `check_db`: Checks if the inventory database is running.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Install dependencies:** `make install`
- **Run the server:** `make run`
- **Run tests:** `make test` (uses PHPUnit)
- **Lint code:** `make lint` (syntax check)
- **Type check:** `make type-check` (uses PHPStan)
- **Clean artifacts:** `make clean`

## Project Structure

- `main.php`: Entry point using the MCP PHP SDK to define the server and tools.
- `composer.json`: PHP dependencies.
- `Makefile`: Commands for build, test, and maintenance.
- `tests/`: Directory containing PHPUnit tests.

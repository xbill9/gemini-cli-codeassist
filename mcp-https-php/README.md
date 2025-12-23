# MCP Stdio PHP Server

A simple Model Context Protocol (MCP) server implemented in PHP. This server is designed to communicate over http and serves as a foundational "Hello World" example for PHP-based MCP integrations.

## Overview

This project provides a basic MCP server named `hello-world-server` that exposes a single tool: `greet`. It uses `monolog/monolog` for structured logging to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

- **PHP 8.1+**
- `composer` (PHP Dependency Manager)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-php
    ```

2.  **Install dependencies:**
    ```bash
    make install
    # Or manually:
    composer install
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the http communication.

To run the server manually (starts listening on http):
```bash
php -S 0.0.0.0:8080 main.php
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this:

```json
{
  "mcpServers": {
    "php-hello-world": {
      "command": "php",
      "args": ["/path/to/mcp-https-php/main.php"]
    }
  }
}
```

*Note: Ensure the absolute path is correct and that the `php` command is in your system PATH.*

## Tools

### `greet`
- **Description:** Get a greeting from a local https server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param`.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Install dependencies:** `make install`
- **Run the server:** `make run`
- **Run tests:** `make test` (uses PHPUnit)
- **Lint code:** `make lint` (syntax check)
- **Type check:** `make type-check` (uses PHPStan)
- **Clean artifacts:** `make clean`

## Project Structure

- `main.php`: Entry point using the MCP SDK to define the server and tools.
- `composer.json`: PHP dependencies.
- `Makefile`: Commands for build, test, and maintenance.
- `tests/`: Directory containing PHPUnit tests.

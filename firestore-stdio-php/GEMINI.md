# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **PHP-based Model Context Protocol (MCP) server** for managing a Cymbal Superstore inventory via Google Cloud Firestore. It exposes tools over standard input/output (stdio) for integration with MCP clients.

## Key Technologies

*   **Language:** PHP 8.2+
*   **SDK:** `mcp/sdk` (Model Context Protocol PHP SDK)
    - [GitHub Repository](https://github.com/modelcontextprotocol/php-sdk)
*   **Database:** Google Cloud Firestore
    - [PHP Client Library](https://github.com/googleapis/google-cloud-php-firestore)

## Project Structure

*   `main.php`: The entry point of the MCP server, defining tools and server logic.
*   `Makefile`: Development shortcuts for installing, testing, linting, and type-checking.
*   `composer.json`: Defines PHP dependencies.
*   `tests/`: Contains PHPUnit tests.
*   `README.md`: General project information and usage instructions.

## Development Setup

1.  **Install PHP and Composer:**
    Ensure you have PHP 8.1+ and Composer installed on your system.

2.  **Install Dependencies:**
    ```bash
    make install
    ```

## Running the Server

The server runs over the `stdio` transport.

```bash
php main.php
```

*Note: Since this is an MCP server running over stdio, it is typically spawned by an MCP client rather than run directly by a human.*

## MCP PHP Developer Resources

*   **MCP PHP SDK (GitHub):** [https://github.com/modelcontextprotocol/php-sdk](https://github.com/modelcontextprotocol/php-sdk)
# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **PHP based Model Context Protocol (MCP) server**. It is designed to expose tools (like `greet`) over streaming HTTP  for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** PHP 8.1+
*   **SDK:** `mcp/sdk` (Model Context Protocol SDK)
*   **Transport:** HTTP (Server-Sent Events) via `StreamableHttpTransport`

## Project Structure

*   `main.php`: The entry point for the MCP server.
*   `composer.json`: PHP dependency management.
*   `sessions/`: Directory for storing persistent sessions.

## Development Setup

1.  **Install Dependencies:**
    ```bash
    composer install
    ```

## Running the Server

Start the PHP built-in web server:

```bash
php -S 0.0.0.0:8080 main.php
```

## Connecting an MCP Client

Configure your MCP client to connect to `http://0.0.0.0:8080/mcp` (or the root `/`, depending on how the client discovers endpoints).

## Notes

*   This server uses `FileSessionStore` to persist sessions across PHP requests, storing them in the `sessions/` directory.

# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Ruby based Model Context Protocol (MCP) server**. It is designed to expose tools over a streaming HTTP transport for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** Ruby
*   **SDK:** `mcp` (Model Context Protocol SDK)
    https://github.com/modelcontextprotocol/ruby-sdk
*   **Database:** Google Cloud Firestore
*   **Server:** Puma (via Rack)

## Project Structure

*   `main.rb`: Main entry point (Ruby). Registers tools, initializes Firestore, and starts the HTTP transport using Puma.
*   `Makefile`: Development shortcuts (test, lint, clean).
*   `spec/`: Contains test files.

## Ruby MCP Best Practices

*   **Logging:** Always log to `stderr` (e.g., `LOGGER = Logger.new($stderr)`).
*   **Tool Definition:**
    *   Inherit from `MCP::Tool`.
    *   Define `description` and `input_schema`.
    *   Implement the logic in a class method `self.call`.
    *   Return an `MCP::Tool::Response` containing an array of content blocks (e.g., `{ type: 'text', text: '...' }`).
*   **Modularity:** For larger projects, keep tools in separate files. In this project, they are currently in `main.rb` for simplicity.
*   **Error Handling:** Wrap tool logic in `begin...rescue` blocks or check pre-requisites (like database connectivity) to return meaningful error messages to the client.
*   **Environment Variables:** Use `dotenv` for local development. Ensure sensitive credentials are never committed.

## Firestore Integration

*   **Client:** Uses the `google-cloud-firestore` gem.
*   **Collection:** The primary collection used is `inventory`.
*   **Schema (Inventory):**
    *   `name` (String)
    *   `price` (Number)
    *   `quantity` (Number)
    *   `imgfile` (String)
    *   `timestamp` (Timestamp)
    *   `actualdateadded` (Timestamp)
*   **Operations:**
    *   `seed`: Populates the database with sample data.
    *   `reset`: Clears the `inventory` collection.
    *   `get_products`: Retrieves all items.
    *   `get_product_by_id`: Retrieves a specific item by document ID.
    *   `get_root`: Returns a welcome message.
    *   `check_db`: Checks database connectivity.

## Development Setup

1.  **Install Dependencies:**
    ```bash
    bundle install
    ```
2.  **Environment Variables:** Create a `.env` file with necessary Google Cloud credentials if not using default application credentials.

## Running the Server

The server is configured to run using a streaming HTTP transport on port 8080.

```bash
bundle exec ruby main.rb
```

*Note: Since this is an MCP server, it is typically spawned or connected to by an MCP client.*

## Ruby MCP Developer Resources

*   **MCP Ruby SDK (GitHub):** [https://github.com/modelcontextprotocol/ruby-sdk](https://github.com/modelcontextprotocol/ruby-sdk)
*   **Model Context Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)

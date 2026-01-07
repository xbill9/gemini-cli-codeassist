# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Ruby based Model Context Protocol (MCP) server**. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** Ruby
*   **SDK:** `mcp` (Model Context Protocol SDK)
    https://github.com/modelcontextprotocol/ruby-sdk
*   **Database:** Google Cloud Firestore

## Project Structure

*   `main.rb`: Main entry point (Ruby). Registers tools and starts the stdio transport.
*   `lib/`: Contains tool definitions and the Firestore client.
    *   `firestore_client.rb`: Singleton client for Firestore interactions.
    *   `*_tool.rb`: Individual tool implementations.
*   `Makefile`: Development shortcuts (test, lint, clean).

## Ruby MCP Best Practices

*   **Logging:** Always log to `stderr` (e.g., `Logger.new($stderr)`). `stdout` is exclusively used for MCP JSON-RPC communication.
*   **Tool Definition:**
    *   Inherit from `MCP::Tool`.
    *   Define `description` and `input_schema`.
    *   Implement the logic in a class method `self.call`.
    *   Return an `MCP::Tool::Response` containing an array of content blocks (e.g., `{ type: 'text', text: '...' }`).
*   **Modularity:** Keep tools in separate files under `lib/` to maintain a clean `main.rb`.
*   **Error Handling:** Wrap tool logic in `begin...rescue` blocks to return meaningful error messages to the client instead of crashing the server.
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

## Development Setup

1.  **Install Dependencies:**
    ```bash
    bundle install
    ```
2.  **Environment Variables:** Create a `.env` file with necessary Google Cloud credentials if not using default application credentials.

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
bundle exec ruby main.rb
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Ruby MCP Developer Resources

*   **MCP Ruby SDK (GitHub):** [https://github.com/modelcontextprotocol/ruby-sdk](https://github.com/modelcontextprotocol/ruby-sdk)
*   **Model Context Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
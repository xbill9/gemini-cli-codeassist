# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Ruby based Model Context Protocol (MCP) server**. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** Ruby
*   **SDK:** `mcp` (Model Context Protocol SDK)
    https://github.com/modelcontextprotocol/ruby-sdk

## Project Structure

*   `main.rb`: Main entry point (Ruby).
*   `Makefile`: Development shortcuts (test, lint, clean).

## Development Setup

1.  **Install Dependencies:**
    ```bash
    bundle install
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
bundle exec ruby main.rb
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Ruby MCP Developer Resources

*   **MCP Ruby SDK (GitHub):** [https://github.com/modelcontextprotocol/ruby-sdk](https://github.com/modelcontextprotocol/ruby-sdk)
*   **Model Context Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
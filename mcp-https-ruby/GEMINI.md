# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Ruby based Model Context Protocol (MCP) server**. It is designed to expose tools (like `greet`) over HTTP (SSE) for integration with MCP clients (such as Claude Desktop or Gemini clients).

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

The server is configured to run using the `StreamableHTTPTransport` (SSE) on port 8080 (default).

```bash
bundle exec ruby main.rb
```

*Note: This MCP server listens for HTTP connections (SSE).*

## Best Practices

*   **Tool Definition:**
    *   Define tools as classes inheriting from `MCP::Tool`.
    *   Use `description` and `input_schema` to clearly define the tool's capabilities and parameters.
    *   Implement the execution logic in the `self.call` class method.
    *   Ensure strict typing in `input_schema` to allow the LLM to call the tool correctly.

*   **Logging:**
    *   Always log to `$stderr` to avoid interfering with protocol communication if `stdio` transport is ever used (though this project uses HTTP).
    *   `LOGGER = Logger.new($stderr)` is the convention.

*   **Testing:**
    *   Unit test tools in isolation using `RSpec`.
    *   Verify both the return structure (`MCP::Tool::Response`) and the content.

## Ruby MCP Developer Resources

*   **MCP Ruby SDK (GitHub):** [https://github.com/modelcontextprotocol/ruby-sdk](https://github.com/modelcontextprotocol/ruby-sdk)
*   **Model Context Protocol Documentation:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
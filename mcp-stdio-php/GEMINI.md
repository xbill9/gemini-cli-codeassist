# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **PHP  based Model Context Protocol (MCP) server**  It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** PHP
*   **SDK:** `mcp` (Model Context Protocol SDK)
https://github.com/modelcontextprotocol/php-sdk

## Project Structure

*   `Makefile`: Development shortcuts (test, lint, clean). *Note: Some targets in the Makefile may reference legacy paths and might need adjustment.*

## Development Setup

1.  **Create and activate a virtual environment (optional but recommended):**
    ```bash
    python3 -m venv .venv
    source .venv/bin/activate
    ```

2.  **Install Dependencies:**
    ```bash
    pip install -r requirements.txt
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
python main.py
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Python MCP Developer Resources

*   **MCP Python SDK (GitHub):** [https://github.com/mcp-protocol/mcp-python-sdk](https://github.com/mcp-protocol/mcp-python-sdk)
*   **FastMCP Documentation:** [https://gofastmcp.com/](https://gofastmcp.com/)
*   **`mcp` package on PyPI:** [https://pypi.org/project/mcp/](https://pypi.org/project/mcp/)

## Legacy/mismatched files
*   `Dockerfile`: Currently configured for a Node.js environment. Needs update for Python.
*   `cloudbuild.yaml`: Currently configured for Node.js/npm builds. Needs update for Python.

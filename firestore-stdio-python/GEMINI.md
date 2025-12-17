# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Python-based Model Context Protocol (MCP) server** using the `FastMCP` class from the `mcp` SDK. It is designed to expose tools (like `get_products`, `seed`, `reset`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients). It acts as an **Inventory Server**, integrating with **Google Cloud Firestore**.

## Key Technologies

*   **Language:** Python 3
*   **SDK:** `mcp` (Model Context Protocol SDK)
*   **Library:** `FastMCP` (for simplified server creation)
*   **Logging:** `python-json-logger`
*   **Dependency Management:** `pip` / `requirements.txt`
*   **Database:** Google Cloud Firestore

## Project Structure

*   `main.py`: The entry point of the application. Initializes the `FastMCP` server ("inventory-server") and defines tools.
*   `requirements.txt`: Python dependencies.
*   `Makefile`: Development shortcuts (test, lint, clean).

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
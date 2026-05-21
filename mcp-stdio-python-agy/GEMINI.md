# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Python-based Model Context Protocol (MCP) server** using the `FastMCP` class from the `mcp` SDK. It is designed to expose tools (like `greet`, `get_system_time`, and `get_system_info`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients).

do *NOT* suggest a python venv

## Key Technologies

*   **Language:** Python 3
*   **SDK:** `mcp` (Model Context Protocol SDK)
*   **Library:** `FastMCP` (for simplified server creation)
*   **Logging:** `python-json-logger`
*   **Dependency Management:** `pip` / `requirements.txt`

## Project Structure

*   `main.py`: The entry point of the application. Initializes the `FastMCP` server ("hello-world-server") and defines tools.
*   `test_greet.py`: Unit tests for the MCP server tools (`greet`, `get_system_time`, `get_system_info`).
*   `test_logging.py`: Unit tests for verifying the JSON stderr logger behavior.
*   `requirements.txt`: Python dependencies.
*   `requirements-dev.txt`: Development dependencies (formatting, linting, type-checking, and tests).
*   `Makefile`: Development shortcuts (test, lint, format, type-check, clean).

## Development Setup

1.  **Install Dependencies:**
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

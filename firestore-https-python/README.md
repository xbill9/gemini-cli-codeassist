# MCP HTTP Python Server

A simple Model Context Protocol (MCP) server implemented in Python using `FastMCP`. This server is designed to communicate over `HTTP` and serves as a foundational "Hello World" example for Python-based MCP integrations.

## Overview

This project provides a basic MCP server named `hello-world-server` that exposes a single tool: `greet`. It uses `python-json-logger` for structured logging to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

- **Python 3.10+**
- `pip` (Python Package Installer)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-stdio-python
    ```

2.  **Set up a virtual environment (recommended):**
    ```bash
    python3 -m venv .venv
    source .venv/bin/activate  # On Windows use `.venv\Scripts\activate`
    ```

3.  **Install dependencies:**
    ```bash
    make install
    # Or manually:
    pip install -r requirements.txt
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the HTTP communication.

To run the server manually:
```bash
python main.py
```
The server will start on `http://localhost:5000` by default.

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this:

```json
{
  "mcpServers": {
    "python-hello-world": {
      "url": "http://localhost:5000"
    }
  }
}
```

*Note: Ensure the server is running before the client attempts to connect.*

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param`.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Install dependencies:** `make install`
- **Run the server:** `make run`
- **Run tests:** `make test`
- **Lint code:** `make lint` (requires `flake8`)
- **Format code:** `make format` (requires `black`)
- **Clean artifacts:** `make clean`

## Project Structure

- `main.py`: Entry point using `FastMCP` to define the server and tools.
- `requirements.txt`: Python dependencies.
- `Makefile`: Commands for build, test, and maintenance.
- `test_logging.py`: Unit tests for logging verification.

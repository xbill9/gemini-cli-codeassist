# MCP Stdio Python Server

A simple Model Context Protocol (MCP) server implemented in Python using `FastMCP`. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for Python-based MCP integrations.

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

2.  **Install dependencies:**
    ```bash
    make install
    # Or manually:
    pip install -r requirements.txt
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
python main.py
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), configure it using standard stdio parameters.

> [!IMPORTANT]
> Ensure that you point the command path to the specific `python` environment where your dependencies are installed, and provide the absolute path to your `main.py` entry point.

Example Configuration:
```json
{
  "mcpServers": {
    "python-hello-world": {
      "command": "python",
      "args": ["/path/to/mcp-stdio-python/main.py"]
    }
  }
}
```

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param`.

### `get_system_time`
- **Description:** Get the current system time in ISO-8601 format and human-readable format.
- **Parameters:** None.
- **Returns:** The local and UTC times as a formatted string.

### `get_system_info`
- **Description:** Get current system information including OS details, CPU count, and memory usage.
- **Parameters:** None.
- **Returns:** Structured OS, kernel version, CPU cores, memory status (Total/Free/Available), and Python version details.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Install dependencies:** `make install`
- **Run the server:** `make run`
- **Run tests:** `make test`
- **Lint code:** `make lint` (requires `flake8`)
- **Format code:** `make format` (requires `black`)
- **Type check:** `make type-check` (requires `mypy`)
- **Clean artifacts:** `make clean`

## Project Structure

- `main.py`: Entry point using `FastMCP` to define the server and tools.
- `requirements.txt`: Core Python dependencies.
- `requirements-dev.txt`: Development dependencies.
- `Makefile`: Commands for build, test, and maintenance.
- `test_greet.py`: Unit tests for MCP server tools.
- `test_logging.py`: Unit tests for structured logging verification.

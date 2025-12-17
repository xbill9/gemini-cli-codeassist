# MCP Stdio Python Server

A simple Model Context Protocol (MCP) server implemented in Python using `FastMCP`. This server is designed to communicate over `stdio` and serves as an "Inventory Server" example for Python-based MCP integrations, demonstrating Google Cloud Firestore integration.

## Overview

This project provides an MCP server named `inventory-server` that exposes tools to manage a product inventory. It uses `python-json-logger` for structured logging to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

- **Python 3.10+**
- `pip` (Python Package Installer)
- **Google Cloud Firestore** (credentials configured via environment or default credentials)

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

4.  **Configuration:**
    Ensure you have a `.env` file or environment variables set up for your Google Cloud project if needed.

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
python main.py
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this:

```json
{
  "mcpServers": {
    "python-inventory-server": {
      "command": "python",
      "args": ["/path/to/mcp-stdio-python/main.py"]
    }
  }
}
```

*Note: Ensure the absolute path is correct and that the python command resolves to the environment where dependencies are installed.*

## Tools

### `get_products`
- **Description:** Get a list of all products from the inventory database.
- **Returns:** A list of product dictionaries.

### `get_product_by_id`
- **Description:** Get a single product from the inventory database by its ID.
- **Parameters:**
    - `id` (string): The ID of the product.
- **Returns:** A product dictionary or error message.

### `seed`
- **Description:** Seed the inventory database with sample products.
- **Returns:** Success message.

### `reset`
- **Description:** Clears all products from the inventory database.
- **Returns:** Success message.

### `get_root`
- **Description:** Get a greeting from the Cymbal Superstore Inventory API.
- **Returns:** Welcome message string.

### `check_db`
- **Description:** Checks if the inventory database is running.
- **Returns:** Database status string.

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
- `test_main.py`: Unit tests.

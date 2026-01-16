# MCP HTTP Haskell Server

A simple Model Context Protocol (MCP) server implemented in Haskell. This server communicates over HTTP (SSE) and serves as a foundational example for Haskell-based MCP integrations.

## Prerequisites

- **GHC 9.10.1+**
- **Cabal 3.12+**

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-haskell
    ```

2.  **Build the project:**
    ```bash
    make build
    # Or manually:
    cabal build
    ```

## Usage

To run the server manually:
```bash
make run
# Or manually:
cabal run mcp-https-haskell
```
The server will start on `http://localhost:8080/mcp`.

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "haskell-mcp": {
      "url": "http://localhost:8080/mcp"
    }
  }
}
```

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `name` (string): The text or name to echo back.
- **Returns:** The string passed in `name`.

## Development

- **Build:** `make build`
- **Run:** `make run`
- **Test:** `make test`
- **Clean:** `make clean`
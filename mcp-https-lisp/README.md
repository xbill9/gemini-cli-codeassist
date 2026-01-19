# MCP HTTPS Common Lisp Server

A simple Model Context Protocol (MCP) server implemented in Common Lisp. This server is designed to communicate over HTTPS (using SSE) and serves as a foundational example for Lisp-based MCP integrations.

## Overview

This project provides a basic MCP server named `mcp-server` that exposes several tools. It uses the `40ants-mcp` library for the protocol implementation.

## Prerequisites

- **SBCL** (Steel Bank Common Lisp).
- **Quicklisp**: The project uses Quicklisp to manage dependencies.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-lisp
    ```

2.  **Install Dependencies:**
    Run the setup script to install Quicklisp (if missing) and all required dependencies.
    ```bash
    make deps
    ```

## Usage

This server is designed to be executed as an HTTP server, listening for SSE connections.

### Building and Running

You can build a standalone binary using the Makefile:

```bash
make build
./mcp-server
```

By default, it listens on port 8080 and binds to 0.0.0.0 (all interfaces), making it suitable for deployment environments like Cloud Run. You can change the port using the `PORT` environment variable:

```bash
PORT=3000 ./mcp-server
```

Or run it directly with `make run`:
```bash
make run
```
(Note: `make run` uses the default port 8080).

### Configuration for MCP Clients

To connect an MCP client (like Claude Desktop) to this server, you would typically use an SSE transport configuration if supported, or use a bridge.

If your client supports SSE directly:

```json
{
  "mcpServers": {
    "lisp-https-mcp": {
      "url": "http://localhost:8080/mcp",
      "transport": "sse"
    }
  }
}
```

## Tools

### `greet`
- **Description:** Get a greeting.
- **Parameters:**
    - `param` (string): The name or text to greet.
- **Returns:** The string passed in `param`.

### `add`
- **Description:** Adds two numbers and returns the result.
- **Parameters:**
    - `a` (integer): First number to add.
    - `b` (integer): Second number to add.
- **Returns:** A string "The sum of <A> and <B> is: <RESULT>".

## Development

The project is structured as a standard Common Lisp system using ASDF.

- **`mcp-https-lisp.asd`**: System definition.
- **`src/`**: Source code.
    - **`packages.lisp`**: Package definition.
    - **`logger.lisp`**: JSON logging utilities.
    - **`main.lisp`**: Entry point and tool definitions.

### Commands

- **Build binary:** `make build`
- **Run binary:** `make run`
- **Run tests:** `make test`
- **Debug mode:** `make debug`
- **Clean artifacts:** `make clean`
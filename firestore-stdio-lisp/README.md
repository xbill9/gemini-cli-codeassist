# MCP Stdio Common Lisp Server

A simple Model Context Protocol (MCP) server implemented in Common Lisp. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for Lisp-based MCP integrations.

## Overview

This project provides a basic MCP server named `hello-world-server` that exposes a single tool: `greet`. It uses the `40ants-mcp` library for the protocol implementation and handles structured logging to stderr, ensuring that the stdout stream remains clean for MCP JSON-RPC messages.

## Prerequisites

- **SBCL** (Steel Bank Common Lisp).
- **Quicklisp**: The project uses Quicklisp to manage dependencies.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-lisp
    ```

2.  **Install Dependencies:**
    Run the setup script to install Quicklisp (if missing) and all required dependencies.
    ```bash
    make deps
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

### Building and Running

You can build a standalone binary using the Makefile:

```bash
make build
./mcp-server
```

Or just run it directly:
```bash
make run
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), point it to the built binary:

```json
{
  "mcpServers": {
    "lisp-hello-world": {
      "command": "/absolute/path/to/firestore-stdio-lisp/mcp-server",
      "args": []
    }
  }
}
```

## Tools

### `greet`
- **Description:** Get a greeting from a local stdio server.
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

- **`firestore-stdio-lisp.asd`**: System definition.
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

# Firestore HTTPS COBOL Server

A Model Context Protocol (MCP) server implemented in **COBOL** (GnuCOBOL) using the `mcpc` library via C bindings. This server communicates over **HTTP** and demonstrates how to build robust MCP servers in COBOL.

## Overview

This project provides an MCP server named `firestore-https-cobol` (binary: `server-cobol`) that exposes a single tool: `greet`. It listens on a TCP port (default 8080) for JSON-RPC messages over HTTP.

## Prerequisites

-   **GnuCOBOL Compiler** (`cobc`)
-   **C Compiler** (GCC, Clang, etc.) supporting C17
-   **Make**
-   **clang-format** (for `make format`)
-   **Python 3** (for integration testing)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-cobol
    ```

2.  **Build the server:**
    ```bash
    make server-cobol
    ```
    *(Note: This will automatically clone the `mcpc` library if it's missing.)*

## Usage

To run the server manually:
```bash
# Default port 8080
./server-cobol

# Or specify a port
PORT=9090 ./server-cobol
```

The server accepts JSON-RPC 2.0 requests via HTTP POST.

### Configuration for MCP Clients

Configure your MCP client to connect via HTTP to the server's URL (e.g., `http://localhost:8080/mcp`).

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): Greeting parameter (e.g., your name).
- **Returns:** A formatted greeting: "Hello, [param]!".

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build COBOL server:** `make server-cobol`
- **Build with Debugging:** `make debug`
- **Build for Release:** `make release`
- **Run COBOL tests:** `make test`
- **Full Quality Check:** `make check` (Lint + Tests)
- **Lint C code:** `make lint`
- **Format C code:** `make format`
- **Clean artifacts:** `make clean`

## Project Structure

- `server.cob`: The main COBOL implementation of the server and tool logic.
- `c_helpers.c`: C helper functions and structured JSON logging.
- `cob_helpers.c`: C wrapper to safely bridge MCP callbacks into COBOL.
- `Makefile`: Build configuration for all components.
- `test_server_http.py`: Integration tests for the COBOL server.
- `mcpc/`: The `mcpc` library source code.

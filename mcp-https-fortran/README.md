# MCP HTTPS Fortran Server

A Model Context Protocol (MCP) server implemented in **Fortran** using the `mcpc` library via C bindings. This server communicates over **HTTP** and demonstrates how to build MCP servers in Fortran.

## Overview

This project provides an MCP server named `mcp-https-fortran` (binary: `server-fortran`) that exposes a single tool: `greet`. It listens on a TCP port (default 8080) for JSON-RPC messages over HTTP.

## Prerequisites

-   **Fortran Compiler** (e.g., `gfortran`)
-   **C Compiler** (GCC, Clang, etc.) supporting C17
-   **Make**
-   **Python 3** (for testing)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-fortran
    # Initialize the mcpc submodule
    git submodule update --init --recursive
    ```

2.  **Build the server:**
    ```bash
    make server-fortran
    ```

## Usage

To run the server manually:
```bash
# Default port 8080
./server-fortran

# Or specify a port
PORT=9090 ./server-fortran
```

The server accepts JSON-RPC 2.0 requests via HTTP POST.

### Configuration for MCP Clients

Configure your MCP client to connect via HTTP to the server's URL (e.g., `http://localhost:8080/mcp`).

## Tools

### `greet`
- **Description:** Get a greeting from the local http server.
- **Parameters:**
    - `param` (string): Greeting parameter.
- **Returns:** A formatted greeting: "Hello, [param]!".

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Build Fortran server:** `make server-fortran`
- **Lint C code:** `make lint`
- **Run Fortran tests:** `make test` (requires Python 3)
- **Format C code:** `make format`
- **Check all:** `make check`
- **Clean artifacts:** `make clean`

## Project Structure

- `server.f90`: The main Fortran implementation of the server and `greet` tool.
- `c_helpers.c`: C helper functions to bridge Fortran and the `mcpc` library.
- `Makefile`: Commands for building C and Fortran binaries.
- `test_server_fortran.py`: Integration tests for the Fortran server.
- `mcpc/`: The C MCP library submodule.

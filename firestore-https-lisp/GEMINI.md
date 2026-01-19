# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Common Lisp based Model Context Protocol (MCP) server**. It is designed to expose tools over **HTTPS (using Server-Sent Events - SSE)** for integration with MCP clients (such as Claude Desktop or Gemini clients), utilizing the `40ants/mcp` system.

The server currently implements the following tools:
*   `greet`: A simple Hello World tool.
*   `add`: A basic arithmetic tool to demonstrate multiple tool support.

## Key Technologies

*   **Language:** Common Lisp
*   **Library:** `40ants/mcp`
    *   Repository: https://github.com/40ants/mcp
*   **Transport:** HTTP/SSE (Server-Sent Events)
*   **Package Manager:** Quicklisp / Ultralisp

## Development Setup

1.  **Prerequisites:**
    *   A Common Lisp implementation (e.g., SBCL).
    *   Quicklisp installed.
    *   (Optional) Roswell for script management.

2.  **Install Dependencies:**
    You will likely need to install the `40ants-mcp` system.
    ```lisp
    (ql:quickload :40ants-mcp)
    ```
    *Note: The project uses a Makefile to manage dependencies and build.*

## Running the Server

The server is configured to run using the `HTTP` transport with SSE.

### Using Make (Preferred)

```bash
make deps   # Install dependencies
make build  # Build the binary 'mcp-server'
./mcp-server
```

Or run directly:

```bash
make run
```

### Configuration

*   **Port:** Default is `8080`. Override with `PORT` environment variable.
    ```bash
    PORT=3000 ./mcp-server
    ```
*   **Binding:** Binds to `0.0.0.0` (all interfaces).
*   **Endpoints:**
    *   `GET /` or `/mcp`: SSE endpoint for events.
    *   `POST /` or `/mcp`: Endpoint for JSON-RPC requests.

## Developer Resources

*   **40ants/mcp GitHub:** [https://github.com/40ants/mcp](https://github.com/40ants/mcp)
*   **Common Lisp HyperSpec:** [http://www.lispworks.com/documentation/HyperSpec/Front/index.htm](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)

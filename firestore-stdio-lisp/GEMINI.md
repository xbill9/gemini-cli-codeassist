# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Common Lisp based Model Context Protocol (MCP) server**. It is designed to expose tools over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients), utilizing the `40ants/mcp` system.

The server currently implements the following tools:
*   `greet`: A simple Hello World tool.
*   `add`: A basic arithmetic tool to demonstrate multiple tool support.

## Key Technologies

*   **Language:** Common Lisp
*   **Library:** `40ants/mcp`
    *   Repository: https://github.com/40ants/mcp
*   **Package Manager:** Quicklisp / Ultralisp

https://github.com/40ants/mcp/tree/master

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
    *Note: You may need to add the Ultralisp dist if it's not in the standard Quicklisp repository.*

## Running the Server

The server is configured to run using the `stdio` transport.

Typical execution involves invoking SBCL directly:

```bash
./main.lisp
# or
sbcl --script main.lisp
# or
make run
```

## Developer Resources

*   **40ants/mcp GitHub:** [https://github.com/40ants/mcp](https://github.com/40ants/mcp)
*   **Common Lisp HyperSpec:** [http://www.lispworks.com/documentation/HyperSpec/Front/index.htm](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)

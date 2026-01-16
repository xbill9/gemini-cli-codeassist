# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **Haskell-based Model Context Protocol (MCP) server** using the `mcp-server` library. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients.

## Key Technologies

*   **Language:** Haskell (GHC 9.10.1+)
*   **Build System:** `cabal`
*   **Libraries:**
    *   `mcp-server`: For implementing the MCP protocol.
    *   `aeson`: For JSON serialization.
    *   `text`: For efficient text handling.

## Project Structure

*   `app/Main.hs`: The entry point of the application. Defines the MCP server and its tools.
*   `mcp-stdio-haskell.cabal`: Cabal configuration file defining dependencies and build targets.
*   `Makefile`: Development shortcuts (build, run, test, clean).

## Development Setup

1.  **Install GHC and Cabal:**
    Ensure you have `ghcup` installed to manage Haskell toolchains.

2.  **Build the Project:**
    ```bash
    cabal build
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
cabal run mcp-stdio-haskell
```

## Haskell MCP Developer Resources

*   **mcp-server package on Hackage:** (Check Hackage for documentation)

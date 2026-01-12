# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

**firestore-stdio-perl** is a **Perl-based Model Context Protocol (MCP) server** using the `MCP::Server` module. It is designed to expose tools (like `greet`) over standard input/output (stdio) for integration with MCP clients (such as Claude Desktop or Gemini clients).

## Key Technologies

*   **Language:** Perl 5 (v5.36+ for signatures)
*   **Library:** `MCP::Server` (for MCP server implementation)
*   **Web Framework:** `Mojolicious` (used for logging and potentially other utilities)
*   **Logging:** `Mojo::Log` (formatted as JSON to STDERR)
*   **Dependency Management:** `cpanm` / `cpanfile`

https://metacpan.org/pod/MCP

## Project Structure

*   `server.pl`: The entry point of the application. Initializes the `MCP::Server` and defines tools.
*   `cpanfile`: Perl dependencies.
*   `local/`: Directory where Perl dependencies are installed (using `cpanm -L local`).
*   `Makefile`: Development shortcuts. *Note: Needs adjustment to match Perl environment.*

## Development Setup

1.  **Install Dependencies:**
    It is recommended to install dependencies locally:
    ```bash
    cpanm --installdeps . -L local
    ```

## Running the Server

The server is configured to run using the `stdio` transport.

```bash
perl -Ilocal/lib/perl5 server.pl
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## Perl MCP Developer Resources

*   **MCP Protocol Specification:** [https://modelcontextprotocol.io/](https://modelcontextprotocol.io/)
*   **Mojolicious Documentation:** [https://docs.mojolicious.org/](https://docs.mojolicious.org/)

## Legacy/mismatched files
*   `Makefile`: Currently contains Python-specific targets. Needs update for Perl (e.g., using `perlcritic`, `prove`).
*   `Dockerfile`: Currently configured for a Node.js environment.
*   `cloudbuild.yaml`: Currently configured for Node.js/npm builds.

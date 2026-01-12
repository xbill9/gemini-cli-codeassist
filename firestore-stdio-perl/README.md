# Firestore Stdio Perl Server

A simple Model Context Protocol (MCP) server implemented in Perl using `MCP::Server` and `Mojolicious`. This server is designed to communicate over `stdio` and serves as a foundational "Hello World" example for Perl-based MCP integrations.

## Overview

This project provides a basic MCP server that exposes a single tool: `greet`. It uses `Mojo::Log` for structured JSON logging to stderr, ensuring that the stdout stream remains clean for the MCP protocol JSON-RPC messages.

## Prerequisites

- **Perl 5.36+** (Recommended for signatures support)
- `cpanm` (App::cpanminus)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-stdio-perl
    ```

2.  **Install dependencies:**
    It is recommended to install dependencies locally to avoid messing with system Perl.
    ```bash
    cpanm --installdeps . -L local
    ```

## Usage

This server is designed to be executed by an MCP client (like Claude Desktop or a Gemini-powered IDE extension) that handles the stdio communication.

To run the server manually (starts listening on stdio):
```bash
perl -Ilocal/lib/perl5 server.pl
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this:

```json
{
  "mcpServers": {
    "perl-hello-world": {
      "command": "perl",
      "args": [
        "-I/path/to/firestore-stdio-perl/local/lib/perl5",
        "/path/to/firestore-stdio-perl/server.pl"
      ]
    }
  }
}
```

*Note: Ensure the absolute path is correct.*

## Tools

### `greet`
- **Description:** Get a greeting from the local server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param`.

## Project Structure

- `server.pl`: Entry point using `MCP::Server` to define the server and tools.
- `cpanfile`: Perl dependencies.
- `local/`: Local library directory for dependencies.
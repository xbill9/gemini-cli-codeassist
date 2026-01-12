# MCP HTTPS Perl Server

A simple Model Context Protocol (MCP) server implemented in Perl using `MCP::Server` and `Mojolicious`. This server is designed to communicate over `HTTP` and serves as a foundational "Hello World" example for Perl-based MCP integrations.

## Overview

This project provides a basic MCP server that exposes a single tool: `greet`. It uses `Mojo::Log` for structured JSON logging to stderr.

## Prerequisites

- **Perl 5.36+** (Recommended for signatures support)
- `cpanm` (App::cpanminus)

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-perl
    ```

2.  **Install dependencies:**
    You can use the provided Makefile:
    ```bash
    make install
    ```
    
    Or manually using `cpanm`:
    ```bash
    cpanm --installdeps . -L local
    ```

## Usage

To run the server (starts listening on http://127.0.0.1:8080):

Using Make:
```bash
make run
```

Or manually:
```bash
perl -Ilocal/lib/perl5 server.pl daemon -l http://0.0.0.0:8080
```

### Configuration for MCP Clients

If you are adding this to an MCP client config (e.g., `claude_desktop_config.json`), the configuration would look something like this (assuming the server is running):

```json
{
  "mcpServers": {
    "perl-hello-world": {
      "url": "http://127.0.0.1:8080/mcp"
    }
  }
}
```

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Run tests:**
  ```bash
  make test
  ```

- **Lint code (Perl::Critic):**
  ```bash
  make lint
  ```

- **Format code (Perl::Tidy):**
  ```bash
  make format
  ```

## Tools

### `greet`
- **Description:** Get a greeting from the local HTTP server.
- **Parameters:**
    - `param` (string): The text or name to echo back.
- **Returns:** The string passed in `param`.

## Project Structure

- `server.pl`: Entry point using `MCP::Server` to define the server and tools.
- `cpanfile`: Perl dependencies.
- `local/`: Local library directory for dependencies.
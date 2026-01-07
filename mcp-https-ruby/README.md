# MCP HTTP/SSE Ruby Server

A simple Model Context Protocol (MCP) server implemented in Ruby using the `mcp` SDK, exposing tools over HTTP (SSE).

## Features

- **greet**: A simple tool that echoes back a message.

## Setup

1.  **Install Ruby**: Ensure you have Ruby 3.0+ installed.
2.  **Install Dependencies**:
    ```bash
    bundle install
    ```
    or
    ```bash
    make install
    ```

## Running the Server

To run the server locally via HTTP (SSE) on port 8080:

```bash
bundle exec ruby main.rb
```
or
```bash
make run
```

The server will be available at `http://localhost:8080`.

## Testing

Run tests with RSpec:

```bash
make test
```

## Linting

Run RuboCop:

```bash
make lint
```

## Development

Use the following `make` commands for common tasks:
- `make all`: Install, test, and lint.
- `make format`: Auto-format code with RuboCop.
- `make clean`: Clean up dependencies and lockfiles.


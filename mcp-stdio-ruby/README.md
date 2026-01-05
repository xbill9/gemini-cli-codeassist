# MCP Stdio Ruby Server

A simple Model Context Protocol (MCP) server implemented in Ruby. This server provides a `greet` tool and uses the `stdio` transport for communication with MCP clients.

## Features

- **greet**: A tool that accepts a message and echoes it back, demonstrating simple tool integration.
- **Logging**: Integrated application logging for debugging and monitoring.

## Prerequisites

- Ruby 3.0 or higher
- Bundler

## Setup

1.  **Clone the repository** (if you haven't already).
2.  **Install Dependencies**:
    ```bash
    make install
    ```
    or
    ```bash
    bundle install
    ```

## Usage

### Running the Server

To start the server using the stdio transport:

```bash
make run
```
or
```bash
bundle exec ruby main.rb
```

### Integrated Tools

#### `greet`
- **Arguments**:
  - `message` (string, required): The message to repeat.
- **Returns**: A text response containing the input message.

## Development

The project uses `rake` for common development tasks:

- **Run Tests**: `bundle exec rake test`
- **Lint Code**: `bundle exec rake lint`
- **Auto-format Code**: `bundle exec rubocop -a`

## License

This project is licensed under the MIT License - see the LICENSE file for details.

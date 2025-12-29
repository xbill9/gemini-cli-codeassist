# MCP HTTPS/SSE C# Server

A simple Model Context Protocol (MCP) server implemented in C# using `ModelContextProtocol.AspNetCore`. This server is designed to communicate over HTTPS using Server-Sent Events (SSE).

## Overview

This project provides a basic MCP server that exposes tools: `Greet`, `GetTime`, and `GetSystemInfo`. It uses ASP.NET Core to provide an SSE-powered MCP interface.

## Prerequisites

- **.NET 10.0 SDK**

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd mcp-https-csharp
    ```

2.  **Restore dependencies:**
    ```bash
    make restore
    ```

3.  **Build the application:**
    ```bash
    make build
    ```

## Usage

To run the server:
```bash
make run
```
The server will start (typically on `http://localhost:5000` or `https://localhost:5001`).

### Configuration for MCP Clients

If you are adding this to an MCP client config, you would use the SSE transport configuration. The endpoint is typically `/mcp`.

Example configuration:
```json
{
  "mcpServers": {
    "csharp-https-server": {
      "url": "http://localhost:5000/mcp/sse"
    }
  }
}
```
*Note: The exact endpoint path might depend on the `ModelContextProtocol.AspNetCore` default mapping. Based on standard behavior, it's often `/mcp/sse` or just `/mcp`.*

## Tools

### `Greet`
- **Description:** Returns a greeting.
- **Parameters:** `name` (string)

### `GetTime`
- **Description:** Returns the current system time.

### `GetSystemInfo`
- **Description:** Returns system specifications and information.

## Development

The project includes a `Makefile` to simplify common development tasks.

- **Restore dependencies:** `make restore`
- **Run the server:** `make run`
- **Build the application:** `make build`
- **Clean artifacts:** `make clean`
# mcp-https-flutter

A Model Context Protocol (MCP) server implemented in Dart, supporting Streamable HTTP transport.

## Features

- **HTTP Transport**: Accessible via HTTP (SSE for downstream, POST for upstream).
- **Native Performance**: Compiles to a native executable for fast startup and low memory footprint.
- **Docker Ready**: Includes a `Dockerfile` for easy containerization and deployment.
- **Cloud Run Support**: Configuration provided for Google Cloud Run deployment.

## Usage

### Command Line Arguments

| Argument | Abbreviation | Default | Description |
|----------|--------------|---------|-------------|
| `--port` | `-p` | `8080` | Port for HTTP transport |
| `--host` | | `0.0.0.0` | Host for HTTP transport |
| `--path` | | `/mcp` | Path for HTTP transport |

### Streamable HTTP Transport

Run the server as an HTTP endpoint:

```bash
dart run bin/mcp_https_flutter.dart --port 8080 --host 0.0.0.0 --path /mcp
```

## Tools

The server provides the following tools:

### `greet`
Takes a string parameter and returns it.
- **Parameters**:
  - `param` (string, required): The parameter to return.

## Key Technologies

*   **Language:** Dart (SDK ^3.10.4)
*   **Protocol:** [Model Context Protocol (MCP)](https://modelcontextprotocol.io/)
*   **SDK:** `mcp_dart`
*   **Dependency Management:** `pub`

## Prerequisites

*   Dart SDK (version 3.10.4 or later)

## Setup

1.  **Get dependencies:**
    ```bash
    dart pub get
    ```

## Running the Server

For local development and testing:

```bash
dart run bin/mcp_https_flutter.dart
```

## Docker & Deployment

### Build Docker Image

```bash
docker build -t mcp-https-flutter .
```

### Run with Docker

```bash
docker run -p 8080:8080 mcp-https-flutter
```

### Google Cloud Run

The project includes `cloudbuild.yaml` for automated deployment to Google Cloud Run. Use the following command to trigger a build and deploy:

```bash
gcloud builds submit --config cloudbuild.yaml --substitutions=_SERVICE_NAME=mcp-https-flutter
```
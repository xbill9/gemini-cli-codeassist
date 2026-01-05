# Cymbal Superstore & MCP Monorepo

This monorepo is an evolution of the **Cymbal Superstore** sample application, showcasing a modern microservices architecture integrated with the **Model Context Protocol (MCP)** and **Google Cloud Platform (GCP)**. It provides a comprehensive set of examples across multiple programming languages for building intelligent, tool-using agents.

## Core Concepts

### Cymbal Superstore
Originally a sample e-commerce application, this project now serves as the foundation for demonstrating:
*   **Inventory Management**: Managing products via Google Cloud Firestore.
*   **Cloud-Native Deployment**: Using Docker, Cloud Build, and Cloud Run.
*   **AI Integration**: Exposing business logic as MCP tools for LLMs (like Gemini and Claude).

### Model Context Protocol (MCP)
The repo contains dozens of MCP server implementations, allowing AI models to interact with the Cymbal Superstore inventory or perform utility tasks.

## Project Categories

### 1. MCP Servers (Inventory & Utilities)
These servers expose tools to MCP clients via Stdio or HTTPS (SSE).

*   **Firestore Inventory Servers (`firestore-*`)**: 
    *   Manage the Cymbal Superstore inventory.
    *   **Languages:** C#, Flutter, Go, Java, Kotlin, PHP, Python, Ruby, Rust, TypeScript.
    *   **Transports:** `stdio` (local), `https` (Cloud Run).
*   **Basic MCP Servers (`mcp-*`)**: 
    *   Simple "Hello World" examples exposing a `greet` tool.
    *   **Languages:** All major languages including Swift and Dart/Flutter.

### 2. Rust & Google Cloud Integrations
Specialized Rust projects for deep GCP integration:
*   **`cloudrun-rust`**: Minimal Cloud Run service.
*   **`pubsub-client-rust`**: Pub/Sub messaging patterns.
*   **`weather-rust` / `log-rust`**: Utility services and logging patterns.
*   **`gcp-client-rust`**: Pattern for generic GCP API calls.

### 3. Experimental & Fun
*   **`battle-royale/`**: Fun Python-based mascot battles and simulations.

## Root Scripts & Tooling

The root directory contains critical scripts for lifecycle management:

| Script | Description |
| :--- | :--- |
| `init.sh` | One-time setup: enables APIs, configures Docker, and sets up Firestore. |
| `set_env.sh` | Exports essential vars like `PROJECT_ID` and `REGION`. (Usage: `source ./set_env.sh`) |
| `backend.sh` | Main build/deploy script for the inventory backend. |
| `backend-open.sh` | Deploys inventory to Cloud Run with public access. |
| `backend-secure.sh` | Deploys inventory to Cloud Run with restricted access. |
| `startproxy.sh` | Starts a local proxy (`gcloud run services proxy`) for secure testing. |
| `enablemcp.sh` | Configures environment for MCP interaction. |

## AI Assistant Integration

Most subdirectories include a `GEMINI.md` file. These files provide specialized context for AI assistants (like Gemini) to understand the specific project's architecture, technologies, and development workflow. If you are using this repo with an AI assistant, it is highly recommended to reference these files.

## Quick Start

### 1. Initialize
```bash
./init.sh
source ./set_env.sh
```

### 2. Local Development (Stdio)
Most directories follow a standard `Makefile` pattern:
```bash
cd firestore-stdio-rust
make build
make run
```

### 3. Deploy to Cloud (HTTPS)
To deploy an MCP server as a containerized service:
```bash
cd firestore-https-ts
make deploy
```

## Directory Naming Convention
*   `firestore-[transport]-[lang]`: Inventory tool using Firestore.
*   `mcp-[transport]-[lang]`: Basic tool implementation.
*   `gcp-[name]-rust`: Specific GCP feature demonstration in Rust.

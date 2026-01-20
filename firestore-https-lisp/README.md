# MCP HTTPS Firestore Lisp Server

A Model Context Protocol (MCP) server implemented in Common Lisp that integrates with Google Cloud Firestore. This server exposes an inventory management API for the "Cymbal Superstore" and communicates over HTTPS (using SSE).

## Overview

This project provides an MCP server named `mcp-server` that allows LLMs to interact with a Firestore-backed inventory system. It uses the `40ants-mcp` library and is designed for easy deployment to platforms like Google Cloud Run.

## Prerequisites

- **SBCL** (Steel Bank Common Lisp).
- **Quicklisp**: Managed via `make deps`.
- **Google Cloud Project**: A Firestore database in native mode.
- **Authentication**: Google Application Default Credentials (ADC) should be configured.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd firestore-https-lisp
    ```

2.  **Install Dependencies:**
    ```bash
    make deps
    ```

## Usage

### Building and Running

Build the standalone binary:

```bash
make build
./mcp-server
```

By default, it listens on port 8080. You can override this:

```bash
PORT=3000 ./mcp-server
```

### Environment Variables

- `PORT`: The port to listen on (default: 8080).
- `GOOGLE_CLOUD_PROJECT`: The GCP Project ID (optional if running on GCP).

## Tools

### `greet`
- **Description:** Get a simple greeting.
- **Parameters:** `param` (string).

### `get_products`
- **Description:** Returns a JSON list of all products in the inventory.

### `get_product_by_id`
- **Description:** Returns a JSON object for a specific product.
- **Parameters:** `id` (string).

### `seed`
- **Description:** Populates the Firestore `inventory` collection with sample data.

### `reset`
- **Description:** Deletes all documents in the `inventory` collection.

### `get_root`
- **Description:** Returns a welcome message from the API.

### `check_db`
- **Description:** Checks if the Firestore connection is working.

### `add`
- **Description:** Adds two numbers.
- **Parameters:** `a` (integer), `b` (integer).

## Development

- **`src/main.lisp`**: Server setup and tool definitions.
- **`src/firestore.lisp`**: Firestore REST API integration logic.
- **`src/logger.lisp`**: JSON logging for Cloud Run compatibility.
# Firestore Inventory MCP Server (Ruby)

A Ruby implementation of a **Model Context Protocol (MCP)** server that connects to **Google Cloud Firestore**. This server exposes tools to manage and inspect a "Cymbal Superstore" inventory database over a streaming HTTP transport.

It is designed to be used with MCP clients (like Claude Desktop or Gemini Code Assist) to enable LLMs to interact with your Firestore data.

## Features & Tools

This server provides the following MCP tools:

*   **`get_root`**: Returns a welcome message and confirms the server is reachable.
*   **`check_db`**: Checks the connectivity status of the Firestore database.
*   **`get_products`**: Retrieves the full list of products from the inventory.
*   **`get_product_by_id`**: Fetches details for a specific product using its document ID.
*   **`seed`**: Populates the Firestore `inventory` collection with sample data (useful for demos/testing).
*   **`reset`**: Clears all documents from the `inventory` collection.

## Prerequisites

*   **Ruby 3.0+**
*   **Google Cloud Project**: You need a Google Cloud project with Firestore enabled.
*   **Authentication**: The environment where this server runs must be authenticated with Google Cloud.
    *   Locally, you can use `gcloud auth application-default login`.
    *   Or set the `GOOGLE_APPLICATION_CREDENTIALS` environment variable to the path of your service account key JSON file.

## Setup

1.  **Clone the repository**:
    ```bash
    git clone <your-repo-url>
    cd firestore-https-ruby
    ```

2.  **Install Dependencies**:
    ```bash
    bundle install
    ```

3.  **Configure Environment**:
    Create a `.env` file if you need to load specific environment variables (handled by `dotenv`).

## Running the Server

This server communicates over **HTTP** using Server-Sent Events (SSE). It is not meant to be run manually in a terminal for human interaction, but rather spawned or connected to by an MCP client.

To start the server:

```bash
bundle exec ruby main.rb
```

The server will start on port 8080 (by default).

### Integration with MCP Clients

Configure your MCP client to connect to the server's HTTP endpoint or run it as a command if the client supports it. For example:

**Command:** `bundle`
**Args:** `['exec', 'ruby', '/path/to/firestore-https-ruby/main.rb']`

## Development

### Testing

Run the test suite using RSpec:

```bash
bundle exec rspec
```

### Linting

Check code style with RuboCop:

```bash
bundle exec rubocop
```

### Building the Gem

This project can also be built as a Ruby gem:

```bash
gem build firestore-mcp-server.gemspec
```
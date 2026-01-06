# Gemini Context: firestore_https_flutter

## Project Overview
This project is a Dart command-line application implementing a **Model Context Protocol (MCP)** server using the `mcp_dart` package. It provides an interface to the Cymbal Superstore Inventory API, backed by Google Cloud Firestore.

## Tech Stack
-   **Language**: Dart (SDK >= 3.10.4)
-   **Key Dependencies**:
    -   `mcp_dart`: For MCP server implementation.
    -   `firedart`: For Firestore interaction in Dart CLI.
    -   `dotenv`: For environment variable management (e.g., `GOOGLE_CLOUD_PROJECT`).
    -   `args`: For parsing command-line arguments.
    -   `path`: For file path manipulation.
-   **Dev Dependencies**:
    -   `lints`: For static analysis and enforcing Dart best practices.
    -   `test`: For unit testing.

## Project Structure
-   `bin/firestore_https_flutter.dart`: The entry point of the application. Sets up the MCP server and handles HTTP configuration.
-   `lib/tools.dart`: Definitions of MCP tools and their handlers, including Firestore logic.
-   `test/`: Unit tests.

## MCP Tools
The server exposes the following tools:
-   `greet`: Get a greeting from the server. (Args: `name`)
-   `get_products`: Get a list of all products from the inventory database.
-   `get_product_by_id`: Get a single product by its ID. (Args: `id`)
-   `search`: Search for products by name. (Args: `query`)
-   `seed`: Seed the inventory database with sample products.
-   `reset`: Clear all products from the inventory database.
-   `get_root`: Get a greeting from the Cymbal Superstore Inventory API.

## Configuration
The server requires the `GOOGLE_CLOUD_PROJECT` environment variable to be set to initialize Firestore. This can be provided via a `.env` file or the environment.

## Development Guidelines

### Coding Style
-   Strictly adhere to the **Dart Style Guide**.
-   Ensure all code passes the configured `lints` (check `analysis_options.yaml`).
-   Use strong typing; avoid `dynamic` unless absolutely necessary.
-   Prefer `final` for variables and fields that do not change.
-   Use `async`/`await` for asynchronous operations.

### MCP Implementation
-   **Tools**: Defined in `bin/firestore_https_flutter.dart` and implemented in `lib/tools.dart`.
-   **Transports**: Supports `http`.
-   **Error Handling**: specific to MCP, return meaningful error codes/messages to the client.
-   **Logging**: Use `stderr` for logging/debug info.

### Testing
-   Write tests for all new tools and logic in `test/`.
-   Run tests using `dart test`.

## Commands
-   **Run**: `dart run --port 8080`
-   **Test**: `dart test`
-   **Analyze**: `dart analyze`
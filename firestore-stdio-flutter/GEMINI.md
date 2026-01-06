# Gemini Context: firestore_stdio_flutter

## Project Overview
This project is a Dart command-line application implementing a **Model Context Protocol (MCP)** server for a fictional "Cymbal Superstore" inventory system. It uses **Firestore** as its database and communicates via standard input/output (stdio).

## Tech Stack
-   **Language**: Dart (SDK ^3.10.4)
-   **Key Dependencies**:
    -   `mcp_dart`: For MCP server implementation.
    -   `firedart`: A Dart-native client for Firestore.
    -   `dotenv`: For managing environment variables.
    -   `path`: For file path manipulation.
-   **Dev Dependencies**:
    -   `lints`: For static analysis and enforcing Dart best practices.
    -   `test`: For unit testing.

## Project Structure
-   `bin/firestore_stdio_flutter.dart`: The entry point. Sets up the MCP server, registers tools, and handles the stdio connection.
-   `lib/`:
    -   `lib/product.dart`: Defines the `Product` data model and Firestore serialization.
    -   `lib/tools.dart`: Contains the implementations of the MCP tools and Firestore interaction logic.
-   `test/`: Contains unit tests for the application.

## MCP Tools
The server exposes the following tools:
-   `get_products`: Returns a list of all products in the inventory.
-   `get_product_by_id`: Returns details for a specific product given its ID.
-   `seed`: Populates the Firestore collection with sample inventory data.
-   `reset`: Deletes all products from the Firestore collection.
-   `get_root`: Returns a welcome message from the Cymbal Superstore Inventory API.
-   `check_db`: Checks if the Firestore connection is initialized.

## Configuration
The application requires a Firestore project ID. This can be provided via environment variables in a `.env` file or from the system environment:
-   `FIRESTORE_PROJECT_ID`
-   `GOOGLE_CLOUD_PROJECT`

## Development Guidelines

### Coding Style
-   Strictly adhere to the **Dart Style Guide**.
-   Ensure all code passes the configured `lints` (check `analysis_options.yaml`).
-   Use strong typing; avoid `dynamic` unless absolutely necessary.
-   Prefer `final` for variables and fields that do not change.
-   Use `async`/`await` for asynchronous operations.

### MCP Implementation
-   **Tools**: Defined in `bin/firestore_stdio_flutter.dart` and implemented in `lib/tools.dart`.
-   **Logging**: Use the custom `log` function in `lib/tools.dart` which writes JSON-formatted logs to `stderr` to avoid corrupting the MCP stdio stream.
-   **Error Handling**: Return meaningful error messages within `CallToolResult` to provide feedback to the MCP client.

### Testing
-   Run tests using `dart test`.

## Commands
-   **Run**: `dart run` (requires environment variables for Firestore)
-   **Test**: `dart test`
-   **Analyze**: `dart analyze`
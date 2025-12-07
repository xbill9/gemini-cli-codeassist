# Firestore Client Rust

This is a Rust command-line interface (CLI) application designed for basic interaction with Google Cloud Firestore. It provides a simple way to manage product data within a Firestore collection named "inventory".

## Features

- **Seed Database**: Populate your Firestore "inventory" collection with sample product data.
- **List Products**: Retrieve and display all products currently stored in the "inventory" collection.
- **Get Product by ID**: Fetch a specific product from the "inventory" collection using its unique ID.

## Technologies Used

- **Rust**: The programming language.
- **Google Cloud Firestore**: NoSQL document database.
- **`firestore` crate**: Rust client library for Firestore.
- **`tokio`**: Asynchronous runtime for Rust.
- **`serde`**: Serialization and deserialization framework.
- **`clap`**: Command-line argument parser.
- **`anyhow`**: Flexible error handling.
- **`dotenv`**: Loads environment variables from a `.env` file.
- **`tracing`**: Application-level tracing for observability.

## Setup and Usage

### Prerequisites

- Rust and Cargo installed.
- Google Cloud Project with Firestore enabled.
- Service Account Key for Firestore authentication (usually handled by `gcloud auth application-default login` or `GOOGLE_APPLICATION_CREDENTIALS` environment variable).

### Environment Variables

Create a `.env` file in the project root with your Google Cloud Project ID:

```
PROJECT_ID=your-gcp-project-id
```

### Building the Application

```bash
cargo build --release
```

### Running Commands

#### Seed the database

```bash
cargo run --release seed
```

#### List all products

```bash
cargo run --release list
```

#### Get a product by ID

Replace `<product_id>` with the actual ID of a product.

```bash
cargo run --release get <product_id>
```

## Project Structure

- `src/main.rs`: Contains the main application logic, including product data structures, Firestore interaction functions, and CLI command handling.
- `Cargo.toml`: Defines project dependencies and metadata.
- `Cargo.lock`: Records the exact versions of dependencies.

## License

This project is licensed under the MIT OR Apache-2.0 License. See the `Cargo.toml` file for more details.

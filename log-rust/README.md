# weather-rust

A simple, containerized Rust web server for fetching weather information, designed for deployment on Google Cloud Run.

## Overview

This project provides a basic "Hello, World!" style web application written in Rust. It serves as a template and example for building and deploying a minimal, secure, and efficient Rust application on Google Cloud Run using Docker and Google Cloud Build.

## Technologies Used

*   **Language:** [Rust](https://www.rust-lang.org/)
*   **Web Framework:** [Hyper](https://hyper.rs/)
*   **Containerization:** [Docker](https://www.docker.com/)
*   **Deployment:** [Google Cloud Run](https://cloud.google.com/run)
*   **CI/CD:** [Google Cloud Build](https://cloud.google.com/build)

## Getting Started

A `Makefile` is included to simplify common development tasks.

### Prerequisites

Before you begin, ensure you have the following installed:

*   [Rust Toolchain](https://www.rust-lang.org/tools/install)
*   [Docker](https://docs.docker.com/get-docker/)
*   [Google Cloud SDK](https://cloud.google.com/sdk/docs/install)

### Initial Setup

Clone the repository and install the necessary dependencies:

```bash
cargo build
```

## Development Workflow

The following commands are available through the `Makefile`.

### Building the Project

*   **Development Build:**
    ```bash
    make build
    ```
*   **Release Build (optimized for production):**
    ```bash
    make release
    ```

### Running Locally

To start the server on `http://localhost:8080`:

```bash
make run
```

### Code Quality

*   **Format the code:**
    ```bash
    make format
    ```
*   **Lint the code with Clippy:**
    ```bash
    make clippy
    ```

### Testing

Run the test suite:

```bash
make test
```

## Deployment

Deployment is automated via Google Cloud Build, as defined in `cloudbuild.yaml`. The process uses a multi-stage `Dockerfile` to build a minimal, secure production image based on `gcr.io/distroless/cc-debian12`.

To manually trigger a deployment, run:

```bash
make deploy
```

This command will:
1.  Submit a build to Google Cloud Build.
2.  Build and push the Docker image to Google Container Registry (GCR).
3.  Deploy the new image to the `cloudrun-rust` service in the `us-central1` region.

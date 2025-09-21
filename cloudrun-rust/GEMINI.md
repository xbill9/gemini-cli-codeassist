# Gemini Workspace for `cloudrun-rust`

This document provides a developer-focused overview of the `cloudrun-rust` project, tailored for use with Gemini.

## Project Overview

`cloudrun-rust` is a basic "Hello, World!" web server written in Rust, designed to be deployed as a containerized application on Google Cloud Run.

### Key Technologies

*   **Language:** [Rust](https://www.rust-lang.org/)
*   **Web Framework:** [Hyper](https://hyper.rs/)
*   **Containerization:** [Docker](https://www.docker.com/)
*   **Deployment:** [Google Cloud Run](https://cloud.google.com/run)
*   **CI/CD:** [Google Cloud Build](https://cloud.google.com/build)

## Getting Started

This project uses a `Makefile` to simplify common development tasks.

### Prerequisites

*   [Rust Toolchain](https://www.rust-lang.org/tools/install)
*   [Docker](https://docs.docker.com/get-docker/)
*   [Google Cloud SDK](https://cloud.google.com/sdk/docs/install)

### Initial Setup

1.  **Install Dependencies:**
    ```bash
    cargo build
    ```

2.  **Run the application locally:**
    ```bash
    make run
    ```
    The server will start on port `8080`.

## Development Workflow

The `Makefile` provides targets for common development tasks.

### Building the Project

*   **Development Build:**
    ```bash
    make build
    ```
*   **Release Build:**
    ```bash
    make release
    ```

### Running Locally

```bash
make run
```

### Code Quality

*   **Formatting:**
    ```bash
    make format
    ```
*   **Linting:**
    ```bash
    make clippy
    ```

### Testing

```bash
make test
```

## Deployment

Deployment is handled by Google Cloud Build and defined in `cloudbuild.yaml`.

### Manual Deployment

To manually trigger a deployment, run:

```bash
make deploy
```

This command submits a build to Google Cloud Build, which will:

1.  Build the Docker image (as defined in `Dockerfile`).
2.  Push the image to Google Container Registry (GCR).
3.  Deploy the new image to the `cloudrun-rust` service in the `us-central1` region.

### Deployment Process

*   **`Dockerfile`**: A multi-stage Dockerfile is used to create a minimal, secure production image.
    1.  **Builder Stage:** The Rust code is compiled in a `rust` builder image.
    2.  **Final Stage:** The compiled binary is copied to a minimal `gcr.io/distroless/cc-debian12` image.
*   **`cloudbuild.yaml`**: This file defines the Cloud Build pipeline. It takes care of building, pushing, and deploying the container image.

## Interacting with Gemini

You can use Gemini to help you with various tasks in this project. Here are some examples:

*   "Add a new endpoint to `main.rs` that returns the current time."
*   "Write a unit test for the new endpoint."
*   "Explain the `Dockerfile` to me."
*   "What does the `clippy` command do?"

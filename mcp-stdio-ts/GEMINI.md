# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a TypeScript-based backend service for the Cymbal Superstore application. It is designed to be deployed as a serverless container on Google Cloud Run.

## Key Technologies

*   **Language:** TypeScript
*   **Framework:** Express.js
*   **Deployment:** Google Cloud Run (via Docker)
*   **Package Manager:** npm
The typescript language spec is here:
https://github.com/microsoft/TypeScript

The MCP Typescript spec is here:
https://github.com/modelcontextprotocol/typescript-sdk/tree/main

## Development Setup

1.  **Install Dependencies:**
    ```bash
    npm install
    ```

2.  **Run in Development Mode:**
    ```bash
    npm run dev
    ```
    This command starts the TypeScript compiler in watch mode and runs the server with `nodemon`, which will automatically restart on file changes.

## Deployment

The application is deployed to Google Cloud Run using a `cloudbuild.yaml` configuration. The deployment process is as follows:

1.  A Docker image is built using the `Dockerfile` in the root directory.
2.  The image is pushed to Google Container Registry (GCR).
3.  The image is deployed to a Cloud Run service.

To trigger a new deployment, push your changes to the main branch of the repository.

## Scripts

The following scripts are available in `package.json`:

*   `clean`: Removes the `dist` directory.
*   `build`: Installs dependencies and compiles the TypeScript code.
*   `start`: Builds and runs the application.
*   `dev`: Runs the application in development mode with hot-reloading.
*   `test`: Runs the test suite.

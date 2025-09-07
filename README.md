# Cymbal Superstore

This project is a sample microservices application for an e-commerce store called "Cymbal Superstore". It demonstrates how to build and deploy a modern, cloud-native application using Google Cloud services.

The Sample Cymbal Super Store files are here:

gs://duet-appdev/cymbal-superstore

## Project Structure

The project is organized into several microservices and components:

-   **`frontend`**: A React-based web application that provides the user interface for the superstore.
-   **`backend`**: A backend service that exposes an inventory API. It is containerized using Docker and deployed on Google Cloud Run.
-   **`functions`**: Contains serverless functions, likely for handling specific business logic or events.
-   **`gateway`**: An API gateway to route requests to the appropriate microservices.
-   **`terraform`**: Infrastructure as Code (IaC) to provision and manage the necessary Google Cloud resources.
-   **`scripts`**: Utility scripts for tasks like environment setup, building, and deploying the application.

## Technologies Used

-   **Frontend**: React, React Router, Bootstrap
-   **Backend**: Python (likely, based on common use with Google Cloud), Docker
-   **Infrastructure**: Google Cloud Platform (GCP)
    -   **Compute**: Google Cloud Run for serving the backend application.
    -   **Database**: Google Firestore for the application's database.
    -   **CI/CD**: Google Cloud Build for continuous integration and deployment.
    -   **Container Registry**: Google Artifact Registry to store Docker images.
-   **Infrastructure as Code**: Terraform

## Shell Scripts

This project includes several shell scripts to facilitate setup, deployment, and management of the application.

### `init.sh`

This script initializes the Google Cloud environment for the project. It performs the following actions:

*   Prompts the user for their Google Cloud project ID.
*   Enables the required Google Cloud services (Cloud Run, Artifact Registry, Cloud Build, etc.).
*   Configures Docker authentication with Google Cloud.
*   Creates a Firestore database.
*   Handles authentication for different environments (Google Cloud Shell, local machine, etc.).

**Usage:**

```bash
./init.sh
```

### `set_env.sh`

This script sets various Google Cloud related environment variables required for the project. It reads the project ID from a local file and exports variables like `PROJECT_ID`, `PROJECT_NUMBER`, `SERVICE_ACCOUNT_NAME`, `GOOGLE_CLOUD_LOCATION`, `REGION`, and `ID_TOKEN`.

**Important:** This script must be sourced, not executed directly, to make the environment variables available in your current shell.

**Usage:**

```bash
source ./set_env.sh
```

### `backend.sh`

This script builds the backend Docker image, pushes it to Google Artifact Registry, and deploys it to Google Cloud Run.

**Usage:**

```bash
./backend.sh
```

## Getting Started

1.  **Initialize the environment:**

    ```bash
    ./init.sh
    ```

2.  **Set the environment variables:**

    ```bash
    source ./set_env.sh
    ```

3.  **Deploy the backend:**

    ```bash
    ./backend.sh
    ```

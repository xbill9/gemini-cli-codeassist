# adk-on-cloudrun

This project implements a "Zoo Guide Agent" using the Google Agent Development Kit (ADK). The agent can answer questions about animals, combining information from a local zoo database (via an MCP server) and Wikipedia. It's designed to be deployed on Google Cloud Run.

## Project Details

The agent is built with Python and the `google-adk` library. It leverages LangChain for Wikipedia integration. The core of the project is a multi-agent system that processes user queries in a sequential workflow.

### Key Dependencies
- `google-adk`: The core framework for building the agent.
- `langchain-community`: Used for integrating with Wikipedia.
- `wikipedia`: Python library for accessing Wikipedia content.

## Agent Architecture

The "Zoo Guide Agent" is composed of a root agent (`greeter`) that delegates to a sequential workflow (`tour_guide_workflow`). This workflow consists of two specialized agents:

1.  **Comprehensive Researcher:** This agent is responsible for gathering information to answer the user's query. It uses two tools:
    *   **MCP Tool:** Connects to a "Zoo MCP Server" to fetch internal data about animals, such as their names, ages, and locations within the zoo. This requires the `MCP_SERVER_URL` environment variable to be set and uses an ID token for authentication.
    *   **Wikipedia Tool:** Uses the `WikipediaQueryRun` tool from LangChain to search for general knowledge about animals, such as their diet, habitat, and lifespan.

2.  **Response Formatter:** This agent takes the data collected by the `comprehensive_researcher` and synthesizes it into a friendly, conversational response. It acts as the voice of the "Zoo Tour Guide," presenting the information in an engaging and easy-to-understand manner.

The entire process is orchestrated by a root agent that greets the user, captures their request, and then initiates the research and response generation workflow.

## Shell Scripts

This project includes several shell scripts to facilitate setup, deployment, and management of the Cloud Run service.

### `init.sh`

This script initializes the Google Cloud environment for the project. It performs the following actions:

*   Prompts the user for their Google Cloud project ID and saves it to `~/project_id.txt`.
*   Sets the Google Cloud project in the `gcloud` configuration.
*   Enables the required Google Cloud services (Cloud Run, Artifact Registry, Cloud Build, etc.).
*   Adds the necessary IAM roles for the user.
*   Handles authentication for different environments (Google Cloud Shell, local machine, etc.).

**Usage:**

```bash
./init.sh
```

### `set_env.sh`

This script sets various Google Cloud related environment variables required for the project. It reads the project ID from `~/project_id.txt` and exports variables like `PROJECT_ID`, `PROJECT_NUMBER`, `SERVICE_ACCOUNT_NAME`, `GOOGLE_CLOUD_LOCATION`, `REGION`, and `ID_TOKEN`.

**Important:** This script must be sourced, not executed directly, to make the environment variables available in your current shell.

**Usage:**

```bash
source ./set_env.sh
```

### `run.sh`

This script runs the agent locally using the ADK CLI. It sources the environment variables from `set_env.sh` before running the agent.

**Usage:**

```bash
./run.sh
```

### `cli.sh`

This script provides an alternative way to run the agent locally. It also sources the environment variables and then runs the agent from within the `zoo_guide_agent` directory.

**Usage:**

```bash
./cli.sh
```

### `cloudrun.sh`

This script deploys the application to Google Cloud Run. The service will be named `zoo-guide-agent` and will be publicly accessible.

**Usage:**

```bash
./cloudrun.sh
```

### `cloudrun-secure.sh`

This script deploys the application to Google Cloud Run with a secure configuration. The service will be named `zoo-guide-agent`, will not be publicly accessible, and requests will need to be authenticated with an identity token.

**Usage:**

```bash
./cloudrun-secure.sh
```

## Deployment

To deploy the application to Google Cloud Run, follow these steps:

1.  **Initialize the environment:**

    ```bash
    ./init.sh
    ```

2.  **Set the environment variables:**

    ```bash
    source ./set_env.sh
    ```

3.  **Deploy the application:**

    *   For public access:

        ```bash
        ./cloudrun.sh
        ```

    *   For a secure deployment:

        ```bash
        ./cloudrun-secure.sh
        ```
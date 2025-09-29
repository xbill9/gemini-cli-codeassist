#!/bin/bash

# This script sets various Google Cloud related environment variables.
# It must be SOURCED to make the variables available in your current shell.
# Example: source ./set_env.sh

# --- Configuration ---
# Set your base configuration here
PROJECT_FILE="~/project_id.txt"
GOOGLE_CLOUD_LOCATION="us-central1"
# ---------------------


echo "--- Setting Google Cloud Environment Variables ---"

# --- Authentication Check ---
echo "Checking gcloud authentication status..."
# Run a command that requires authentication. Redirect output so it's clean.
if gcloud auth print-access-token > /dev/null 2>&1; then
  echo "gcloud is authenticated."
else
  echo "Error: gcloud is not authenticated."
  echo "Please log in by running: gcloud auth login"
  # Use 'return' instead of 'exit' because the script is meant to be sourced.
  return 1
fi
# --- --- --- --- --- ---


# 1. Check if project file exists and set Project ID
PROJECT_FILE_PATH=$(eval echo $PROJECT_FILE) # Expand potential ~
if [ ! -f "$PROJECT_FILE_PATH" ]; then
  echo "Error: Project file not found at $PROJECT_FILE_PATH"
  echo "Please create $PROJECT_FILE_PATH containing your Google Cloud project ID."
  return 1
fi
PROJECT_ID_FROM_FILE=$(cat "$PROJECT_FILE_PATH")
echo "Setting gcloud config project to: $PROJECT_ID_FROM_FILE"
gcloud config set project "$PROJECT_ID_FROM_FILE" --quiet

# --- Export Core GCP Identifiers ---
export PROJECT_ID=$(gcloud config get project)
export GOOGLE_CLOUD_PROJECT=$(gcloud config get project)
export PROJECT_NUMBER=$(gcloud projects describe ${PROJECT_ID} --format="value(projectNumber)")
export SERVICE_ACCOUNT_NAME=$(gcloud compute project-info describe --format="value(defaultServiceAccount)")

echo "Exported PROJECT_ID=$PROJECT_ID"
echo "Exported PROJECT_NUMBER=$PROJECT_NUMBER"
echo "Exported SERVICE_ACCOUNT_NAME=$SERVICE_ACCOUNT_NAME"
echo "Exported GOOGLE_CLOUD_PROJECT=$GOOGLE_CLOUD_PROJECT"

# --- Export Location and Region ---
export GOOGLE_CLOUD_LOCATION="$GOOGLE_CLOUD_LOCATION"
export REGION="$GOOGLE_CLOUD_LOCATION" # Use the same value for REGION
echo "Exported GOOGLE_CLOUD_LOCATION=$GOOGLE_CLOUD_LOCATION"
echo "Exported REGION=$REGION"

# --- Export Application-Specific Variables ---

export ID_TOKEN=$(gcloud auth print-identity-token)

# Vertex AI / GenAI
export GOOGLE_GENAI_USE_VERTEXAI="TRUE"
echo "Exported GOOGLE_GENAI_USE_VERTEXAI=$GOOGLE_GENAI_USE_VERTEXAI"

# short SHA for cloud build
export SHORT_SHA=$(git rev-parse --short HEAD)

# set rust trace level
export RUST_LOG=trace

echo ""
echo "--- Environment setup complete ---"

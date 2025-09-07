

#!/bin/bash

# --- Function for error handling ---
handle_error() {
  echo "Error: $1"
  exit 1
}

# --- Part 1: Set Google Cloud Project ID ---



PROJECT_FILE="$HOME/project_id.txt"
echo "--- Setting Google Cloud Project ID File ---"

read -p "Please enter your Google Cloud project ID: " user_project_id

if [[ -z "$user_project_id" ]]; then
  handle_error "No project ID was entered."
fi

echo "You entered: $user_project_id"
echo "$user_project_id" > "$PROJECT_FILE"

if [[ $? -ne 0 ]]; then
  handle_error "Failed saving your project ID: $user_project_id."
fi

source ./set_env.sh
gcloud config set project $(cat ~/project_id.txt) 
echo "Successfully saved project ID."


echo "Enabling Services"
gcloud services enable \
    run.googleapis.com \
    artifactregistry.googleapis.com \
    cloudbuild.googleapis.com \
    aiplatform.googleapis.com \
    compute.googleapis.com 

echo "Hardcoding Region to europe-west1" 

gcloud config set compute/region europe-west1


echo "Adding IAM Roles"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:$SERVICE_ACCOUNT" \
  --role="roles/run.invoker" \
    --quiet \
    --condition=None
    

export GOOGLE_CLOUD_PROJECT=$(gcloud config get project)

  if  [[ -z "$CLOUD_SHELL" ]] && curl -s -i metadata.google.internal | grep -q "Metadata-Flavor: Google"; then
     echo "This VM is running on GCP Defaults to Service Account."
  fi 

if [ "$CLOUD_SHELL" = "true" ]; then
  echo "Running in Google Cloud Shell."
else
  if curl -s -i metadata.google.internal | grep -q "Metadata-Flavor: Google"; then
     echo "This VM is running on Google Cloud."
  else
    echo "Not running in Google Cloud VM or Shell."
    echo "Setting ADC Credentials"
    gcloud auth application-default login
  fi
fi

if [ -n "$FIREBASE_DEPLOY_AGENT" ]; then
echo "Running in Firebase Studio terminal"
else
echo "Not running in Firebase Studio terminal"
fi

if [ -d "/mnt/chromeos" ] ; then
     echo "Running on ChromeOS"
else
      echo "Not running on ChromeOS"
fi

export ID_TOKEN=$(gcloud auth print-identity-token)

export MCP_SERVER_URL=https://zoo-mcp-server-${PROJECT_NUMBER}.europe-west1.run.app/mcp/
echo -e "\nMCP_SERVER_URL=https://zoo-mcp-server-${PROJECT_NUMBER}.europe-west1.run.app/mcp/" > ./zoo_guide_agent/.env
echo -e "\nMODEL=gemini-2.5-flash" >> ./zoo_guide_agent/.env
echo -e "\nSERVICE_ACCOUNT=${PROJECT_NUMBER}-compute@developer.gserviceaccount.com" >> ./zoo_guide_agent/.env
echo "Setting MCP SERVER URL $MCP_SERVER_URL"

MODEL="gemini-2.5-flash"
SERVICE_ACCOUNT="${PROJECT_NUMBER}-compute@developer.gserviceaccount.com"

export MODEL="gemini-2.5-flash"
echo "Setting MODEL $MODEL"

echo "--- Initial Setup complete ---"


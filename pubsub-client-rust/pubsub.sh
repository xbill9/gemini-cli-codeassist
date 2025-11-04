

#!/bin/bash

# --- Function for error handling ---
handle_error() {
  echo "Error: $1"
  exit 1
}

# --- Part 1: Set Google Cloud Project ID ---



PROJECT_FILE="$HOME/project_id.txt"

source ../set_env.sh
gcloud config set project $(cat ~/project_id.txt) 
export PROJECT_ID=$(gcloud config get project)
export PROJECT_NUMBER=$(gcloud projects describe ${PROJECT_ID} --format="value(projectNumber)")
export GOOGLE_CLOUD_PROJECT=$(gcloud config get project)
export SERVICE_ACCOUNT="${PROJECT_NUMBER}-compute@developer.gserviceaccount.com"
echo "Successfully saved project ID."


echo "Enabling Pubsub Services"
gcloud services enable pubsub.googleapis.com

echo "Hardcoding Region to us-central1" 
gcloud config set compute/region us-central1

echo "Adding IAM Roles"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:$SERVICE_ACCOUNT" \
  --role="roles/run.invoker" \
    --quiet \
    --condition=None
    
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:$SERVICE_ACCOUNT" \
  --role="roles/pubsub.admin" \
    --quiet \
    --condition=None

# check runtime
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

export MODEL="gemini-2.5-pro"
echo "Setting MODEL $MODEL"


echo "--- Creating Topic and Subscription ---"
if ! gcloud pubsub topics describe my-topic >/dev/null 2>&1; then
  echo "Creating Pub/Sub topic 'my-topic'..."
  gcloud pubsub topics create my-topic
else
  echo "Pub/Sub topic 'my-topic' already exists."
fi

if ! gcloud pubsub subscriptions describe my-sub >/dev/null 2>&1; then
  echo "Creating Pub/Sub subscription 'my-sub' for topic 'my-topic'..."
  gcloud pubsub subscriptions create my-sub --topic my-topic
else
  echo "Pub/Sub subscription 'my-sub' already exists."
fi

echo "--- Initial Setup complete ---"


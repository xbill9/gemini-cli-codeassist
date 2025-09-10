source ~/gemini-cli-codeassist/set_env.sh

echo "Deploying to Cloud Run Unauthenticated"
gcloud run deploy inventory --image=gcr.io/$PROJECT_ID/cymbal-superstore-inventory-api --port=8000 --region=us-central1 --allow-unauthenticated




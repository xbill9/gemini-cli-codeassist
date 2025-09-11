source ~/gemini-cli-codeassist/set_env.sh
echo "Building Backend "

cd ~/gemini-cli-codeassist/cymbal-superstore/backend
docker build -t gcr.io/$PROJECT_ID/cymbal-superstore-inventory-api:latest .

echo "Building Docker Image"
docker push gcr.io/$PROJECT_ID/cymbal-superstore-inventory-api:latest

echo "Deploying to Cloud Run"
gcloud run deploy inventory --image=gcr.io/$PROJECT_ID/cymbal-superstore-inventory-api --port=8000 --region=us-central1 --allow
-unauthenticated




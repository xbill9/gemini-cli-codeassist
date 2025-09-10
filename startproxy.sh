source ~/gemini-cli-codeassist/set_env.sh

echo "Starting Local Proxy"
gcloud run services proxy inventory --region us-central1 --port=3000




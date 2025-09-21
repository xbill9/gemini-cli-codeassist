source ~/gemini-cli-codeassist/set_env.sh
echo "Building Cloud Run Rust "

cd ~/gemini-cli-codeassist/cloudrun-rust
docker build -t gcr.io/$PROJECT_ID/cloudrun-rust:$SHORT_SHA

echo "Building Docker Image"
docker push gcr.io/$PROJECT_ID/cloudrun-rust:$SHORT_SHA

echo "Deploying to Docker"
docker run gcr.io/$PROJECT_ID/cloudrun-rust:$SHORT_SHA




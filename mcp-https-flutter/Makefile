.PHONY: get run test clean build lint

get:
	dart pub get

run:
	dart run bin/mcp_https_flutter.dart --transport http --port 8080 --host 0.0.0.0 --path /mcp

test:
	dart test

lint:
	dart analyze

clean:
	rm -rf .dart_tool build .packages .pub bin/mcp_https_flutter

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml



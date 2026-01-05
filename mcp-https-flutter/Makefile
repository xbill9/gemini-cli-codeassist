.PHONY: get run test clean build lint release check

get:
	dart pub get

run:
	dart run bin/mcp_https_flutter.dart --port 8080 --host 0.0.0.0 --path /mcp

check: lint test

test:
	dart test

lint:
	dart analyze

build:
	dart compile exe bin/mcp_https_flutter.dart -o bin/mcp_https_flutter

release: lint test build

clean:
	rm -rf .dart_tool build .packages .pub bin/mcp_https_flutter

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml



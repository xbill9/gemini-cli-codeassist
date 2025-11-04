# Makefile for the Rust project

# Variables
SERVICE_NAME := pubsub-client-rust
REGION := us-central1
PROJECT_ID := $(shell gcloud config get-value project) # Cache project ID

.PHONY: all build run clean release test format check-fmt clippy check docker-build deploy publish doc help

# The default target
all: build

# Build the project for development
build:
	@echo "Building the Rust project..."
	@cargo build

# Run the project
run:
	@echo "Running the Rust project..."
	PROJECT_ID=$(PROJECT_ID) cargo run

# Clean the project
clean:
	@echo "Cleaning the project..."
	@cargo clean

# Build the project for release
release:
	@echo "Building Release..."
	@cargo build --release

# Run tests
test:
	@echo "Running tests..."
	@cargo test

# Format the code
format:
	@echo "Formatting code..."
	@cargo fmt --all

# Check formatting
check-fmt:
	@echo "Checking formatting..."
	@cargo fmt --all -- --check

# Lint the code
clippy:
	@echo "Linting code..."
	@cargo clippy -- -D warnings

# Check the code
check:
	@echo "Checking the code..."
	@cargo check

# Publish the crate
publish:
	@echo "Publishing the crate..."
	@cargo publish

update:
	@cargo update --verbose

# Generate documentation
doc:
	@echo "Generating documentation..."
	@cargo doc

# Build the Docker image
docker-build:
	@echo "Building the Docker image..."
	@docker build -t gcr.io/$(PROJECT_ID)/$(SERVICE_NAME):latest .

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml

help:
	@echo "Makefile for the Rust project"
	@echo ""
	@echo "Usage:"
	@echo "    make <target>"
	@echo ""
	@echo "Targets:"
	@echo "    all          (default) same as 'build'"
	@echo "    build        Build the project for development"
	@echo "    run          Run the project"
	@echo "    clean        Clean the project"
	@echo "    release      Build the project for release"
	@echo "    test         Run tests"
	@echo "    format       Format the code"
	@echo "    check-fmt    Check code formatting"
	@echo "    clippy       Lint the code"
	@echo "    check        Check the code"
	@echo "    publish      Publish the crate"
	@echo "    doc          Generate documentation"
	@echo "    docker-build Build the Docker image"
	@echo "    deploy       Submit the build to Google Cloud Build"

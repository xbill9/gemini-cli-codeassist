# Makefile for the Go project

# Variables
SERVICE_NAME := firestore-stdio-go
REGION := us-central1
PROJECT_ID := $(shell gcloud config get-value project) # Cache project ID
APP_DIR := .

.PHONY: all build run clean release test format check-fmt lint check deps doc docker-build deploy help

# The default target
all: build

# Build the project for development
build:
	@echo "Building the Go project..."
	@cd $(APP_DIR) && go build .

# Run the project
run:
	@echo "Running the Go project..."
	@cd $(APP_DIR) &&  go run main.go

# Clean the project
clean:
	@echo "Cleaning the project..."
	@cd $(APP_DIR) && go clean

# Build the project for release
release:
	@echo "Building Release..."
	@cd $(APP_DIR) && CGO_ENABLED=0 GOOS=linux go build -ldflags="-s -w" -trimpath -v -o $(SERVICE_NAME) .

# Run tests
test:
	@echo "Running tests..."
	@cd $(APP_DIR) && go test -v ./...

# Format the code
format:
	@echo "Formatting code..."
	@gofmt -w $(APP_DIR)

# Check formatting
check-fmt:
	@echo "Checking formatting..."
	@test -z $(shell gofmt -l $(APP_DIR)) || (echo "gofmt found files that need formatting"; exit 1)

# Lint the code
lint:
	@echo "Linting code..."
	@cd $(APP_DIR) && go vet ./...

# Check the code
check: lint

# Update dependencies
deps:
	@echo "Updating dependencies..."
	@cd $(APP_DIR) && go get -u ./... && go mod tidy

# Generate documentation
doc:
	@echo "Generating documentation..."
	@echo "Use 'godoc -http=:6060' to view documentation."

# Build the Docker image
docker-build:
	@echo "Building the Docker image..."
	@docker build -t gcr.io/$(PROJECT_ID)/$(SERVICE_NAME):latest .

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml

help:
	@echo "Makefile for the Go project"
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
	@echo "    lint         Lint the code"
	@echo "    check        Alias for lint"
	@echo "    deps         Update dependencies"
	@echo "    doc          Show how to generate documentation"
	@echo "    docker-build Build the Docker image"
	@echo "    deploy       Submit the build to Google Cloud Build"

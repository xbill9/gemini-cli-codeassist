# Makefile for the Rust project

# ==============================================================================
# Variables
# ==============================================================================

# Service name for Cloud Run
SERVICE_NAME := firestore-rust
# Google Cloud Region
REGION := us-central1
# Cargo binary
CARGO := cargo

# ==============================================================================
# Phony Targets
# ==============================================================================

.PHONY: all build run clean release test fmt format clippy deploy help workspace doc

# ==============================================================================
# Core Development Targets
# ==============================================================================

all: build ## (Default) Build the project for development.

build: ## Build the project for development.
	@echo "Building the Rust project..."
	@$(CARGO) build

run: ## Run the project.
	@echo "Running the Rust project..."
	@$(CARGO) run

clean: ## Clean the project.
	@echo "Cleaning the project..."
	@$(CARGO) clean

release: ## Build the project for release.
	@echo "Building Release..."
	@$(CARGO) build --release

# ==============================================================================
# Code Quality & Testing
# ==============================================================================

test: ## Run tests.
	@echo "Running tests..."
	@$(CARGO) test

fmt: ## Check if the code is correctly formatted.
	@echo "Checking formatting..."
	@$(CARGO) fmt --all -- --check

format: ## Format the code automatically.
	@echo "Formatting code..."
	@$(CARGO) fmt --all

clippy: ## Lint the code for common issues.
	@echo "Linting code..."
	@$(CARGO) clippy -- -D warnings

# ==============================================================================
# Deployment & Documentation
# ==============================================================================

deploy: ## Submit the build to Google Cloud Build.
	@echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml --substitutions=_SERVICE_NAME=$(SERVICE_NAME),_REGION=$(REGION),SHORT_SHA=$(SHORT_SHA)

workspace: ## Publish workspace (if applicable).
	@echo "Publishing Workspace..."
	@$(CARGO) publish --workspace

doc: ## Generate documentation.
	@echo "Building Docs..."
	@$(CARGO) doc

# ==============================================================================
# Help
# ==============================================================================

help: ## Display this help screen.
	@echo "Makefile for the Rust project"
	@echo ""
	@echo "Usage:"
	@echo "    make <target>"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "    %-20s %s\n", $1, $2}'


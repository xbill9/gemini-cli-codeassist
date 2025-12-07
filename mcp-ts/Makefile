# Makefile for the TypeScript project

# ==============================================================================
# Variables
# ==============================================================================

# Service name for Cloud Run
SERVICE_NAME := mcp-ts
# Google Cloud Region
REGION := us-central1
# NPM command
NPM := npm

# ==============================================================================
# Phony Targets
# ==============================================================================

.PHONY: all build start dev clean test audit deploy help

# ==============================================================================
# Core Development Targets
# ==============================================================================

all: build ## (Default) Build the project for development.

build: ## Build the project for development.
	@echo "Building the TypeScript project..."
	@$(NPM) run build

start: ## Run the project.
	@echo "Running the TypeScript project..."
	@$(NPM) start

dev: ## Run the project in development mode.
	@echo "Running the TypeScript project in development mode..."
	@$(NPM) run dev

clean: ## Clean the project.
	@echo "Cleaning the project..."
	@$(NPM) run clean

# ==============================================================================
# Code Quality & Testing
# ==============================================================================

lint: ## Run linting.
	@echo "Running linting..."
	@$(NPM) run lint

test: ## Run tests.
	@echo "Running tests..."
	@$(NPM) test

audit: ## Run npm audit to check for vulnerabilities.
	@echo "Running npm audit..."
	@$(NPM) audit

# ==============================================================================
# Deployment
# ==============================================================================

deploy: ## Submit the build to Google Cloud Build.
	@echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml --substitutions=_SERVICE_NAME=$(SERVICE_NAME),_REGION=$(REGION),SHORT_SHA=$$(git rev-parse --short HEAD)

# ==============================================================================
# Help
# ==============================================================================

help: ## Display this help screen.
	@echo "Makefile for the TypeScript project"
	@echo ""
	@echo "Usage:"
	@echo "    make <target>"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "    %-20s %s\n", $1, $2}'

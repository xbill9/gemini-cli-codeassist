# Makefile for mcp-https-haskell

.PHONY: all build run debug test clean help

all: build

# Target to build the application
build:
	@echo "Building the application..."
	cabal build --allow-newer

# Target to run the application
run:
	@echo "Running the application..."
	cabal run mcp-https-haskell --allow-newer

# Target to run the application in debug mode
debug:
	@echo "Running the application in debug mode..."
	cabal run mcp-https-haskell --allow-newer --ghc-options="-g -O0"

# Target to run tests
test:
	@echo "Running tests..."
	cabal test --allow-newer

# Target to clean build artifacts
clean:
	@echo "Cleaning up..."
	cabal clean

# Target to build for release
release:
	@echo "Building for release..."
	cabal install --install-method=copy --installdir=./bin --allow-newer

# Submit the build to Google Cloud Build
deploy:
	echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml



# Help target
help:
	@echo "Available targets:"
	@echo "  build   - Build the Haskell application"
	@echo "  run     - Run the Haskell application"
	@echo "  debug   - Run the Haskell application in debug mode"
	@echo "  test    - Run tests"
	@echo "  clean   - Clean build artifacts"
	@echo "  help    - Show this help message"

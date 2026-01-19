# Makefile for firestore-stdio-haskell

.PHONY: all build run test clean help

all: build

# Target to build the application
build:
	@echo "Building the application..."
	cabal build --allow-newer

# Target to run the application
run:
	@echo "Running the application..."
	cabal run firestore-stdio-haskell --allow-newer

# Target to run tests
test:
	@echo "Running tests..."
	cabal test --allow-newer

# Target to clean build artifacts
clean:
	@echo "Cleaning up..."
	cabal clean

# Help target
help:
	@echo "Available targets:"
	@echo "  build   - Build the Haskell application"
	@echo "  run     - Run the Haskell application"
	@echo "  test    - Run tests"
	@echo "  clean   - Clean build artifacts"
	@echo "  help    - Show this help message"
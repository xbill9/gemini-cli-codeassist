# Makefile for firestore-stdio-swift

.PHONY: all install run build test lint format clean check release

all: build

# Target to build the application in release mode
release:
	@echo "Building in release mode..."
	@swift build -c release

# Target to perform all checks (lint and test)
check: lint test

# Target to fetch dependencies
install:
	@echo "Fetching dependencies..."
	@swift package resolve

# Target to run the application
run:
	@echo "Running the application..."
	@swift run firestore-stdio-swift

# Target to build the application
build:
	@echo "Building the application..."
	@swift build

# Target to run tests
test:
	@echo "Running tests..."
	@swift test

# Target to lint the code
lint:
	@echo "Linting code..."
	@swift-format lint -r Sources Tests

# Target to format the code
format:
	@echo "Formatting code..."
	@swift-format format -i -r Sources Tests

# Target to clean build artifacts
clean:
	@echo "Cleaning up..."
	@rm -rf .build
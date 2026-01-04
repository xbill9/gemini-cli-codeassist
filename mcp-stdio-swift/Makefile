# Makefile for mcp-stdio-swift

.PHONY: all install run build test clean

all: build

# Target to fetch dependencies
install:
	@echo "Fetching dependencies..."
	@swift package resolve

# Target to run the application
run:
	@echo "Running the application..."
	@swift run mcp-stdio-swift

# Target to build the application
build:
	@echo "Building the application..."
	@swift build

# Target to run tests
test:
	@echo "Running tests..."
	@swift test

# Target to clean build artifacts
clean:
	@echo "Cleaning up..."
	@rm -rf .build
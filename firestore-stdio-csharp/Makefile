# Makefile for mcp-stdio-csharp

.PHONY: all restore run build test format lint clean status pull push

all: build

# Target to restore dependencies
restore:
	@echo "Restoring dependencies..."
	@dotnet restore

# Target to run the application
run:
	@echo "Running the application..."
	@dotnet run

# Target to build the application
build:
	@echo "Building the application..."
	@dotnet build

# Target to run tests
test:
	@echo "Running tests..."
	@echo "No tests defined for this project yet."
	# @dotnet test

# Target to format the code
format:
	@echo "Formatting the code..."
	@dotnet format

# Target to lint the code
lint:
	@echo "Linting the code..."
	@dotnet format --verify-no-changes

# Target to show git status
status:
	@echo "Showing git status..."
	@git status

# Target to pull latest changes from git
pull:
	@echo "Pulling latest changes from git..."
	@git pull

# Target to push changes to git
push:
	@echo "Pushing changes to git..."
	@git push

.PHONY: clean
clean:
	@echo "Cleaning up..."
	@dotnet clean

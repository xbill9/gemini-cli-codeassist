# Makefile for mcp-stdio-python

.PHONY: all install run build test lint format status pull push clean

all: install test lint

# Target to install dependencies
install:
	@echo "Installing dependencies..."
	@pip install -r requirements.txt

# Target to run the application
run:
	@echo "Running the application..."
	@python main.py

# Target to build the application (placeholder for future use)
build:
	@echo "Building the application..."
	@echo "No build steps defined for this project, currently just runs Python code."

# Target to run tests
test:
	@echo "Running tests..."
	@python -m pytest test_logging.py test_greet.py

# Target to lint the code
lint:
	@echo "Linting the code..."
	@flake8 .

# Target to format the code
format:
	@echo "Formatting the code..."
	@black .

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
	@find . -type f -name "*.pyc" -delete
	@find . -type d -name "__pycache__" -exec rm -rf {} +
	@find . -type d -name ".pyte*" -exec rm -rf {} +
	@rm -rf .venv

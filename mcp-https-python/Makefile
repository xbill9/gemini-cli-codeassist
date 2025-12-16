# Makefile for mcp-stdio-python

.PHONY: all install run build test lint format type-check status pull push clean

all: install lint type-check

# Target to install dependencies
install:
	@echo "Installing dependencies..."
	@pip install -r requirements.txt

release:
	@echo "Installing dependencies..."
	@pip install -r requirements.txt
	@python main.py

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

# Target to type-check the code
type-check:
	@echo "Type-checking the code..."
	@mypy .

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

# Build the Docker image
docker-build:
	@echo "Building the Docker image..."
	@docker build -t gcr.io/$(PROJECT_ID)/$(SERVICE_NAME):latest .

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml

.PHONY: clean
clean:
	@echo "Cleaning up..."
	@find . -type f -name "*.pyc" -delete
	@find . -type d -name "__pycache__" -exec rm -rf {} +
	@find . -type d -name ".pyte*" -exec rm -rf {} +
	@find . -type d -name ".mypy*" -exec rm -rf {} +
	@rm -rf .venv

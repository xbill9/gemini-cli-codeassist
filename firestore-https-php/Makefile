# Makefile for firestore-https-php

.PHONY: all install run build test lint format type-check status pull push clean

all: install test lint

# Target to install dependencies
install:
	@echo "Installing dependencies..."
	@php composer.phar install
	@php composer.phar update

# Target to run the application
run:
	@echo "Starting PHP built-in server..."
	@php -S 0.0.0.0:8080 main.php

# Target to build the application (placeholder)
build:
	@echo "Building the application..."
	@echo "No build steps defined for this project."

# Target to run tests
test:
	@echo "Running tests..."
	@vendor/bin/phpunit tests

# Target to lint the code (Syntax check)
lint:
	@echo "Linting the code..."
	@find . -name "*.php" ! -path "./vendor/*" -exec php -l {} \;

# Target to format the code
format:
	@echo "Formatting the code..."
	@echo "No formatter configured. Consider using php-cs-fixer."

# Target to type-check the code
type-check:
	@echo "Type-checking the code..."
	@vendor/bin/phpstan analyse

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
	@rm -rf vendor

deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml


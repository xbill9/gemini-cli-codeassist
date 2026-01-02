# Makefile for mcp-stdio-ruby

.PHONY: all install run build test lint format type-check status pull push clean

all: install test lint

# Target to install dependencies
install:
	@echo "Installing dependencies..."
	@bundle install

# Target to run the application
run:
	@echo "Running the application..."
	@bundle exec ruby main.rb

release:
	@echo "Running the application..."
	@bundle exec ruby main.rb

# Target to build the application (placeholder)
build:
	@echo "Building the application..."
	@echo "No build steps defined for this project."

# Target to run tests
test:
	@echo "Running tests..."
	@bundle exec rspec

# Target to lint the code
lint:
	@echo "Linting the code..."
	@bundle exec rubocop

# Target to format the code
format:
	@echo "Formatting the code..."
	@bundle exec rubocop -a

# Target to type-check the code
type-check:
	@echo "Type-checking the code..."
	@echo "Ruby is dynamic; consider using Sorbet or Steep if needed."

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
	@rm -rf vendor .bundle Gemfile.lock

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml


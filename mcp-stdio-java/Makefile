# Makefile for mcp-stdio-java

.PHONY: all install run build test clean status pull push lint

all: install build test

# Target to install dependencies
install:
	@echo "Installing dependencies..."
	@./mvnw dependency:resolve

# Target to run the application using Maven (WARNING: May pollute stdout with build logs)
run:
	@echo "Running the application via Maven..."
	@./mvnw spring-boot:run

# Target to run the compiled jar (recommended for MCP clients to avoid stdout pollution)
run-jar: build
	@java -jar target/mcp-stdio-java-0.0.1-SNAPSHOT.jar

# Target to build the application
build:
	@echo "Building the application..."
	@./mvnw clean package -DskipTests

# Target to build the application
release:
	@echo "Building the application..."
	@./mvnw clean package -DskipTests

# Target to run tests
test:
	@echo "Running tests..."
	@./mvnw test

# Target to run linting
lint:
	@echo "Running linting..."
	@./mvnw checkstyle:check

# Target to clean the project
clean:
	@echo "Cleaning up..."
	@./mvnw clean

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

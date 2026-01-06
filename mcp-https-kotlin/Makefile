.PHONY: all build run test clean lint format

GRADLE_CMD ?= ./gradlew

all: build

check: build lint test

build:
	$(GRADLE_CMD) build

run:
	$(GRADLE_CMD) run

test:
	$(GRADLE_CMD) test

lint:
	$(GRADLE_CMD) ktlintCheck

format:
	$(GRADLE_CMD) ktlintFormat

clean:
	$(GRADLE_CMD) clean

install:
	$(GRADLE_CMD) installDist	

release: install build run

deploy:
	gcloud builds submit . --config cloudbuild.yaml

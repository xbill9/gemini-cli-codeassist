.PHONY: all build run test clean

GRADLE_CMD ?= ./gradlew

all: build

build:
	$(GRADLE_CMD) build

run:
	$(GRADLE_CMD) run

test:
	$(GRADLE_CMD) test

clean:
	$(GRADLE_CMD) clean

install:
	$(GRADLE_CMD) installDist	

deploy:
	gcloud builds submit . --config cloudbuild.yaml

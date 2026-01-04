.PHONY: all build run test clean lint format check

GRADLE_CMD ?= ./gradlew

all: build

build:
	$(GRADLE_CMD) build

run:
	$(GRADLE_CMD) run

test:
	$(GRADLE_CMD) test

check: lint test

lint:
	$(GRADLE_CMD) ktlintCheck

format:
	$(GRADLE_CMD) ktlintFormat

clean:
	$(GRADLE_CMD) clean

install:
	$(GRADLE_CMD) installDist

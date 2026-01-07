.PHONY: all build run test clean lint format check

GRADLE_CMD ?= ./gradlew

all: build

build:
	$(GRADLE_CMD) build installDist

run:
	$(GRADLE_CMD) run

test:
	$(GRADLE_CMD) test

check: test lint

clean:
	$(GRADLE_CMD) clean

install:
	$(GRADLE_CMD) installDist

lint:
	$(GRADLE_CMD) ktlintCheck

format:
	$(GRADLE_CMD) ktlintFormat

release: format lint build test 

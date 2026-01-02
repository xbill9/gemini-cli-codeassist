.PHONY: all build run test clean

GRADLE_CMD ?= ./gradlew

all: build

build:
	$(GRADLE_CMD) build installDist

run:
	$(GRADLE_CMD) run

test:
	$(GRADLE_CMD) test

clean:
	$(GRADLE_CMD) clean

install:
	$(GRADLE_CMD) installDist

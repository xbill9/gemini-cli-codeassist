ZIG = zig
BUILD_DIR = zig-out
BIN_NAME = mcp-https-zig
BIN = $(BUILD_DIR)/bin/$(BIN_NAME)
SYMLINK = server

.PHONY: all build debug clean test format lint

all: build

build:
	$(ZIG) build
	@rm -f $(SYMLINK)
	@ln -s $(BIN) $(SYMLINK)
	@echo "Linked $(BIN) to $(SYMLINK)"

debug:
	$(ZIG) build -Doptimize=Debug
	@rm -f $(SYMLINK)
	@ln -s $(BIN) $(SYMLINK)
	@echo "Linked $(BIN) to $(SYMLINK)"

release:
	$(ZIG) build -Doptimize=ReleaseSafe
	@rm -f $(SYMLINK)
	@ln -s $(BIN) $(SYMLINK)
	@echo "Linked $(BIN) to $(SYMLINK)"

clean:
	rm -rf zig-out zig-cache $(SYMLINK)

test: build
	$(ZIG) build test
	python3 test_server.py

format:
	$(ZIG) fmt .

lint:
	$(ZIG) fmt --check .
	$(ZIG) build -Doptimize=ReleaseFast

deploy:
	echo "Submitting build to Google Cloud Build..."
	gcloud builds submit . --config cloudbuild.yaml



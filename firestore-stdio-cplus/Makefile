.PHONY: all debug release test check lint format clean

BUILD_DIR = build
TARGET = server

all: release

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

release: $(BUILD_DIR)
	cmake -B $(BUILD_DIR) -S . -DCMAKE_BUILD_TYPE=Release
	cmake --build $(BUILD_DIR)
	cp $(BUILD_DIR)/$(TARGET) .

debug: $(BUILD_DIR)
	cmake -B $(BUILD_DIR) -S . -DCMAKE_BUILD_TYPE=Debug
	cmake --build $(BUILD_DIR)
	cp $(BUILD_DIR)/$(TARGET) .

test:
	python3 test_server.py

check: lint test

lint:
	clang-format --dry-run --Werror main.cpp firestore_client.cpp firestore_client.hpp

format:
	clang-format -i main.cpp firestore_client.cpp firestore_client.hpp

clean:
	rm -rf $(BUILD_DIR) $(TARGET)

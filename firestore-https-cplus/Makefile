CXX = c++
BASE_CXXFLAGS = -std=c++17 -Wall -Wextra -Icpp-mcp/include -Icpp-mcp/common -DCPPHTTPLIB_OPENSSL_SUPPORT -DMCP_SSL
CXXFLAGS = $(BASE_CXXFLAGS) -O2
MCP_LIB = cpp-mcp/build/src/libmcp.a
LDFLAGS = $(MCP_LIB) -lpthread -lssl -lcrypto

all: server

# Debug build: includes debug info, no optimization
debug: CXXFLAGS = $(BASE_CXXFLAGS) -g -O0 -DDEBUG
debug: clean server

# Release build: full optimization, no debug info
release: CXXFLAGS = $(BASE_CXXFLAGS) -O3 -DNDEBUG
release: clean server
	strip server

HEADERS = $(wildcard cpp-mcp/include/*.h) $(wildcard cpp-mcp/common/*.hpp)

$(MCP_LIB):
	mkdir -p cpp-mcp/build
	cd cpp-mcp/build && cmake -DMCP_SSL=ON .. && $(MAKE) mcp

server: main.o firestore_client.o $(MCP_LIB)
	$(CXX) -o server main.o firestore_client.o $(LDFLAGS)

main.o: main.cpp $(HEADERS)
	$(CXX) $(CXXFLAGS) -c main.cpp

firestore_client.o: firestore_client.cpp firestore_client.hpp $(HEADERS)
	$(CXX) $(CXXFLAGS) -c firestore_client.cpp

clean:
	rm -f server main.o firestore_client.o
	rm -rf cpp-mcp/build

test: server
	python3 test_server.py

run: server
	./server

check: lint test

lint:
	clang-format --dry-run --Werror main.cpp firestore_client.cpp firestore_client.hpp

format:
	clang-format -i main.cpp firestore_client.cpp firestore_client.hpp

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml

.PHONY: all clean test check lint format debug release run deploy submodule-update

# Target to pull the main repository and update all submodules
submodule-update:
	@echo "Pulling main repository changes..."
	git pull origin $(shell git rev-parse --abbrev-ref HEAD)
	@echo "Updating all submodules to the latest remote versions..."
	git submodule update --init --recursive --remote

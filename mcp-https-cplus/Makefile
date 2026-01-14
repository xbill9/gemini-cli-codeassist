CXX = c++
BASE_CXXFLAGS = -std=c++17 -Wall -Wextra -Icpp-mcp/include -Icpp-mcp/common
CXXFLAGS = $(BASE_CXXFLAGS) -O2
LDFLAGS = cpp-mcp/build/src/libmcp.a -lpthread

all: libmcp server

# Build the cpp-mcp library if it doesn't exist
libmcp:
	@if [ ! -f cpp-mcp/build/src/libmcp.a ]; then \
		echo "Building cpp-mcp library..."; \
		mkdir -p cpp-mcp/build && \
		cd cpp-mcp/build && \
		cmake -DCMAKE_BUILD_TYPE=Release .. && \
		make -j$(nproc); \
	fi

# Debug build: includes debug info, no optimization
debug: CXXFLAGS = $(BASE_CXXFLAGS) -g -O0 -DDEBUG
debug: clean libmcp server

# Release build: full optimization, no debug info
release: CXXFLAGS = $(BASE_CXXFLAGS) -O3 -DNDEBUG
release: clean libmcp server
	strip server

HEADERS = $(wildcard cpp-mcp/include/*.h) $(wildcard cpp-mcp/common/*.hpp)

server: main.o
	$(CXX) -o server main.o $(LDFLAGS)

main.o: main.cpp $(HEADERS)
	$(CXX) $(CXXFLAGS) -c main.cpp

lint:
	clang-format --dry-run --Werror main.cpp

format:
	clang-format -i main.cpp

clean:
	rm -f server main.o

test: server
	python3 test_server.py

run: server
	./server

check: lint test

# Submit the build to Google Cloud Build
deploy:
	@echo "Submitting build to Google Cloud Build..."
	@gcloud builds submit . --config cloudbuild.yaml



.PHONY: all clean test check lint format debug release run

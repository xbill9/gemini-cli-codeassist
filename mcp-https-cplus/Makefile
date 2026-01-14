CXX = c++
BASE_CXXFLAGS = -std=c++17 -Wall -Wextra -Icpp-mcp/include -Icpp-mcp/common
CXXFLAGS = $(BASE_CXXFLAGS) -O2
LDFLAGS = cpp-mcp/build/src/libmcp.a -lpthread

all: server

# Debug build: includes debug info, no optimization
debug: CXXFLAGS = $(BASE_CXXFLAGS) -g -O0 -DDEBUG
debug: clean server

# Release build: full optimization, no debug info
release: CXXFLAGS = $(BASE_CXXFLAGS) -O3 -DNDEBUG
release: clean server
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

.PHONY: all clean test check lint format debug release run

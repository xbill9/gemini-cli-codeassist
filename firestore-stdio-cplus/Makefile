all: server

server:
	cmake -B build -S . -DCMAKE_BUILD_TYPE=Release
	cmake --build build --target server -j $(nproc)
	cp build/server .

debug:
	cmake -B build -S . -DCMAKE_BUILD_TYPE=Debug
	cmake --build build --target server -j $(nproc)
	cp build/server .

clean:
	rm -rf build server

test: server
	python3 test_server.py

lint:
	clang-format --dry-run --Werror main.cpp

format:
	clang-format -i main.cpp

check: lint test

.PHONY: all server clean test check lint format
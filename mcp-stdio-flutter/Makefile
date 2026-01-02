.PHONY: get run test clean build

get:
	dart pub get

build:
	dart compile exe bin/mcp_stdio_flutter.dart -o bin/mcp_stdio_flutter

run:
	dart run bin/mcp_stdio_flutter.dart

test:
	dart test

clean:
	rm -rf .dart_tool build .packages .pub bin/mcp_stdio_flutter

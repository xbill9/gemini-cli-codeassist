.PHONY: get run test clean build lint release check

get:
	dart pub get


check: lint test


lint:
	dart analyze

release: lint test build


build:
	dart pub get && dart compile exe bin/firestore_stdio_flutter.dart -o bin/firestore_stdio_flutter

run:
	dart run bin/firestore_stdio_flutter.dart

test:
	dart test

clean:
	rm -rf .dart_tool build .packages .pub bin/firestore_stdio_flutter

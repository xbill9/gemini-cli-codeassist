# Use the official Dart SDK image as a build stage
FROM dart:stable AS build

# Set the working directory
WORKDIR /app

# Copy pubspec files and get dependencies
COPY pubspec.* ./
RUN dart pub get

# Copy the rest of the application code
COPY . .

# Ensure dependencies are up to date and build the executable
RUN dart pub get --offline
RUN dart compile exe bin/firestore_https_flutter.dart -o bin/server

# Use a slim runtime image
FROM debian:bookworm-slim

# Copy the compiled executable from the build stage
COPY --from=build /app/bin/server /app/bin/server

# Expose the port the app runs on
EXPOSE 8080

# Run the application
# We use --host 0.0.0.0 to listen on all interfaces as required by Cloud Run
# We use a shell to expand the PORT environment variable
CMD ["sh", "-c", "/app/bin/server --transport http --port ${PORT:-8080} --host 0.0.0.0 --path /mcp"]

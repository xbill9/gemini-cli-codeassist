# Build stage
FROM debian:bookworm AS build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the entire project
COPY . .

# Build the application (this will also build cpp-mcp as per Makefile)
RUN make release

# Deploy stage
# Use distroless cc image for C++ applications
FROM gcr.io/distroless/cc-debian12:nonroot

WORKDIR /app

# Copy the binary from the build stage
COPY --from=build /app/server /app/server

# Expose the port (informative)
EXPOSE 8080

# Run the application as non-root user
ENTRYPOINT ["/app/server"]

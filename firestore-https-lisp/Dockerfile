# Build stage
FROM debian:bookworm-slim AS build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    sbcl \
    curl \
    ca-certificates \
    build-essential \
    git \
    libpthread-stubs0-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the source code
COPY . .

# Install dependencies and build the application
RUN make deps && make build

# Deploy stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    libpthread-stubs0-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the binary from the build stage
COPY --from=build /app/mcp-server /app/server

# Expose the port (informative)
EXPOSE 8080

# Run the application
ENTRYPOINT ["/app/server"]
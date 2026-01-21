# Build stage
FROM debian:bookworm-slim AS build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    gnucobol \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the source code
COPY . .

# Build the application
RUN make release

# Deploy stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libcob4 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the binary from the build stage
COPY --from=build /app/server-cobol /app/server-cobol

# Expose the port (informative)
EXPOSE 8080

# Run the application
ENTRYPOINT ["/app/server-cobol"]
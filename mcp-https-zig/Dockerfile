# Build stage
FROM debian:bookworm-slim AS build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    curl \
    xz-utils \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install Zig 0.15.2
RUN curl -L https://ziglang.org/download/0.15.2/zig-x86_64-linux-0.15.2.tar.xz | tar -xJ -C /usr/local
ENV PATH="/usr/local/zig-x86_64-linux-0.15.2:${PATH}"

WORKDIR /app

# Copy the source code
COPY . .

# Build the application
RUN zig build -Doptimize=ReleaseSafe

# Deploy stage
FROM debian:bookworm-slim

WORKDIR /app

# Copy the binary from the build stage
COPY --from=build /app/zig-out/bin/mcp-https-zig /app/server

# Expose the port (informative)
EXPOSE 8080

# Run the application
ENTRYPOINT ["/app/server"]

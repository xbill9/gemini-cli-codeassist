# Build stage
FROM haskell:9.12-bookworm AS build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    libffi-dev \
    libgmp-dev \
    libnuma-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy cabal files and local dependencies first for caching
COPY cabal.project ./
COPY mcp-https-haskell.cabal ./
COPY deps ./deps

# Update cabal and build dependencies
RUN cabal update && cabal build --only-dependencies --allow-newer

# Copy the rest of the source code
COPY . .

# Build the application
# We use cabal install to copy the binary to a known location
RUN cabal install --install-method=copy --installdir=/app/bin --allow-newer --overwrite-policy=always

# Deploy stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi8 \
    libnuma1 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the binary from the build stage
COPY --from=build /app/bin/mcp-https-haskell /app/mcp-https-haskell

# Expose the port (informative, Cloud Run uses PORT env var)
EXPOSE 8080

# Run the application
CMD ["/app/mcp-https-haskell"]

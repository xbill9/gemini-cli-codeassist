# Build stage
FROM debian:bookworm AS build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the entire project
COPY . .

# Build the application
RUN make release

# Deploy stage
FROM gcr.io/distroless/cc-debian12:nonroot

WORKDIR /app

# Copy the binary from the build stage
COPY --from=build /app/server /app/server

# Expose the port (informative)
EXPOSE 8080

# Environment variables
ENV PORT=8080

# Run the application
ENTRYPOINT ["/app/server"]

# Dockerfile for a Rust project with a multi-stage build for production.

# --- Builder Stage ---
# This stage builds the Rust binary.
FROM rust:1.82-slim as builder

# Set the working directory
WORKDIR /usr/src/mcp-rust

# Copy the manifests
COPY Cargo.toml Cargo.lock ./

# Create a dummy main.rs to build dependencies
RUN mkdir src && echo "fn main() {}" > src/main.rs

# Build the dependencies
RUN cargo build --release

# Remove the dummy main.rs
RUN rm -rf src

# Copy the actual source code
COPY src ./src

# Build the application
RUN cargo build --release

# --- Runtime Stage ---
# This stage creates the final, small, and secure image.
FROM gcr.io/distroless/cc-debian12

# Copy the compiled binary from the builder stage
COPY --from=builder /usr/src/mcp-rust/target/release/mcp-rust /usr/local/bin/mcp-rust

# Expose the port the application will run on
EXPOSE 8080

# Set the entrypoint for the container
ENTRYPOINT ["/usr/local/bin/mcp-rust"]

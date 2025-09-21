# ==============================================================================
# Builder Stage
# ==============================================================================
# Use a specific version of the Rust image as the builder.
# Using buster as it's a common and stable base.
FROM rust as builder

# Set the working directory.
WORKDIR /usr/src/cloudrun-rust

# Copy the dependency manifests.
COPY Cargo.toml Cargo.lock ./

# Copy the source code.
COPY src ./src

# Build the release binary.
RUN cargo build --release

# ==============================================================================
# Final Stage
# ==============================================================================
# Use a minimal, non-root base image for the final container.
# "distroless" images are a good choice for security as they contain only the
# application and its runtime dependencies.
FROM gcr.io/distroless/cc-debian12

# Set the working directory.
WORKDIR /app

# Copy the compiled binary from the builder stage.
COPY --from=builder /usr/src/cloudrun-rust/target/release/cloudrun-rust .

# Expose the port the app runs on.
# The mapping to the host port is done at runtime.
EXPOSE 8080

# Set the command to run the application.
CMD ["./cloudrun-rust"]
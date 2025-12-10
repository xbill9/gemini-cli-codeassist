# Build stage
FROM golang:1.24.4 AS build

WORKDIR /app

# Copy go.mod and go.sum files
COPY go.mod go.sum ./

# Download dependencies
RUN go mod download

# Copy the source code
COPY . .

# Build the application
# -o /server: Output binary name
# CGO_ENABLED=0: Disable CGO for static linking
# GOOS=linux: Target Linux OS
RUN CGO_ENABLED=0 GOOS=linux go build -o /server

# Deploy stage
# Use distroless static image for smallest size and best security
FROM gcr.io/distroless/static-debian12:nonroot

WORKDIR /

# Copy the binary from the build stage
COPY --from=build /server /server

# Expose the port (informative)
EXPOSE 8080

# Run the application as non-root user (default in this image)
ENTRYPOINT ["/server"]

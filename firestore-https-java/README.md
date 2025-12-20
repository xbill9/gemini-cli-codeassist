# HTTPS MCP Server in Java

This project is a Spring Boot application that implements a Model Context Protocol (MCP) server using the HTTP (SSE) transport.

## Prerequisites

- Java 25 or later
- Maven (wrapper included)

## Building the Project

To build the project, you can use Maven directly:

```bash
./mvnw clean package
```

Alternatively, you can use the provided `Makefile`:

```bash
make build
```

This will generate an executable JAR file in the `target` directory: `target/mcp-https-java-0.0.1-SNAPSHOT.jar`.

## Running the MCP Server

The server listens for MCP connections on HTTP (Server-Sent Events).

### Using the JAR file

```bash
java -jar target/mcp-https-java-0.0.1-SNAPSHOT.jar
```

Or via Makefile:

```bash
make run-jar
```

### Using Maven

```bash
./mvnw spring-boot:run
```

Or via Makefile:

```bash
make run
```

### Endpoints

- **SSE Endpoint (GET):** `http://localhost:8080/mcp/message`
- **Message Endpoint (POST):** `http://localhost:8080/mcp/message`

## Available Tools

The server exposes the following tools:

- `reverseString(input: String)`: Reverses the input string.
- `greet(name: String)`: Greets the user by name.

## Development

### Testing

Run the tests using:

```bash
./mvnw test
```

Or via Makefile:

```bash
make test
```

### Linting

The project uses Checkstyle (Google checks) for code linting. You can run it with:

```bash
./mvnw checkstyle:check
```

Or via Makefile:

```bash
make lint
```

## Configuration

The server is configured in `src/main/resources/application.properties`:

- `spring.application.name=xbill.mcp.https`: The application name.
- `spring.main.web-application-type=reactive`: Enables the reactive web server.
- `spring.ai.mcp.server.name=xbill-https-server`: The MCP server name.
- `spring.ai.mcp.server.version=1.0.0`: The MCP server version.

Logging is configured in `src/main/resources/logback-spring.xml`.
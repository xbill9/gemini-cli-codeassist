# Stdio MCP Server in Java

This project is a Spring Boot application that implements a Model Context Protocol (MCP) server using the Stdio transport.

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

This will generate an executable JAR file in the `target` directory: `target/mcp-stdio-java-0.0.1-SNAPSHOT.jar`.

## Running the MCP Server

The server listens for JSON-RPC messages on standard input and writes responses to standard output.

### Using the JAR file (Recommended)

Running the compiled JAR is recommended for MCP clients to avoid potential pollution of standard output by build tool logs.

```bash
java -jar target/mcp-stdio-java-0.0.1-SNAPSHOT.jar
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

*Note: Running via Maven might include additional output in stdout which can interfere with the MCP protocol.*

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



- `spring.application.name=xbill.mcp.stdio`: The application name.

- `spring.ai.mcp.server.stdio=true`: Enables Stdio transport.

- `spring.main.web-application-type=none`: Disables the web server.

- `spring.main.banner-mode=off`: Disables the startup banner.

- `spring.ai.mcp.server.name=xbill-stdio-server`: The MCP server name.

- `spring.ai.mcp.server.version=1.0.0`: The MCP server version.



Logging is configured in `src/main/resources/logback-spring.xml` to output to `System.err`, ensuring that logs do not interfere with the MCP protocol on `System.out`.

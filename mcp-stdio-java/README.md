# Stdio MCP Server in Java

This project is a Spring Boot application that implements a Model Context Protocol (MCP) server using the Stdio transport.

## Prerequisites

- Java 25 or later
- Maven (wrapper included)

## Building the Project

To build the project, run:

```bash
./mvnw clean package
```

This will generate an executable JAR file in the `target` directory.

## Running the MCP Server

You can run the MCP server using the following command:

```bash
java -jar target/mcp-stdio-java-0.0.1-SNAPSHOT.jar
```

The server listens for JSON-RPC messages on standard input and writes responses to standard output.
Logging to the console is disabled to avoid interfering with the protocol.

## Available Tools

The server exposes the following tools:

- `reverseString(input: String)`: Reverses the input string.
- `greet(name: String)`: Greets the user by name.

## Configuration

The server is configured in `src/main/resources/application.properties`:

- `spring.ai.mcp.server.stdio=true`: Enables Stdio transport.
- `spring.main.web-application-type=none`: Disables the web server.
- `spring.main.banner-mode=off`: Disables the startup banner.

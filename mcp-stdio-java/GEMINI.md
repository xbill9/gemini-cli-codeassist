# Java Developer Best Practices

This document outlines the coding standards and best practices for developers working on the `mcp-stdio-java` project.

## Core Principles

- **Maintainability:** Write code that is easy to read, understand, and modify.
- **Testability:** Design components that are easy to unit test in isolation.
- **Consistency:** Follow existing project patterns and naming conventions.

## Coding Standards

### Style Guide
- **Google Java Style:** The project uses the Google Java Style Guide.
- **Linting:** Always run `make lint` (or `./mvnw checkstyle:check`) before committing. Fix all checkstyle violations.

### Modern Java (Java 25)
- **Var:** Use `var` for local variables when the type is obvious from the initializer.
- **Records:** Use `records` for immutable data transfer objects (DTOs).
- **Pattern Matching:** Utilize pattern matching for `switch` and `instanceof`.
- **Text Blocks:** Use text blocks for multi-line strings (e.g., JSON, SQL).

## Spring Boot Practices

- **Constructor Injection:** Always use constructor injection instead of `@Autowired` on fields. This makes components easier to test and ensures they are immutable.
- **Component Scanning:** Rely on `@Component`, `@Service`, and `@Repository` for automatic bean discovery.
- **Externalized Configuration:** Use `@Value` or `@ConfigurationProperties` to manage settings in `application.properties`.

## MCP Specifics

- **Standard I/O:** Never print to `System.out` as it is reserved for the MCP JSON-RPC protocol.
- **Logging:** Use SLF4J/Logback. The project is configured to send all logs to `System.err`.
- **Tool Design:** 
    - Keep tools idempotent when possible.
    - Provide clear descriptions in `@Tool` and `@ToolParam` annotations as these are used by the LLM to understand how to use the tool.
    - Handle null or invalid inputs gracefully within the tool implementation.

## Testing

- **JUnit 5:** Use JUnit 5 for all new tests.
- **Mockito:** Use Mockito for mocking dependencies in unit tests.
- **AssertJ:** Use AssertJ for fluent and readable assertions.
- **Coverage:** Aim for high test coverage on core logic, especially for tools.

## Development Workflow

1.  **Understand:** Review requirements and existing code.
2.  **Test-Driven:** Write a failing test before implementing new logic if possible.
3.  **Implement:** Code the feature following the style guide.
4.  **Verify:** Run `make test` and `make lint`.
5.  **Build:** Run `make build` to ensure the JAR can be generated.

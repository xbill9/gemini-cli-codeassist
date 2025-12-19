# Use the official Eclipse Temurin image for a lean production stage of our multi-stage build.
# https://hub.docker.com/_/eclipse-temurin
FROM eclipse-temurin:25-jdk AS builder

WORKDIR /app

# Copy the maven wrapper and pom.xml first to cache dependencies
COPY .mvn/ .mvn
COPY mvnw pom.xml ./

# Install dependencies (this layer will be cached if pom.xml doesn't change)
RUN ./mvnw dependency:go-offline

# Copy the source code
COPY src ./src

# Build the application
RUN ./mvnw package -DskipTests

# Use a smaller JRE image for the final stage
FROM eclipse-temurin:25-jre

WORKDIR /app

# Copy the jar to the production image from the builder stage.
COPY --from=builder /app/target/mcp-https-java-*.jar /app/app.jar

# Service must listen to $PORT environment variable.
ENV PORT=8080

# Run the web service on container startup.
CMD ["java", "-jar", "/app/app.jar"]


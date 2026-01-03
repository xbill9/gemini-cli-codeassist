FROM eclipse-temurin:25-jdk AS builder
WORKDIR /app
COPY . .
RUN chmod +x gradlew
RUN ./gradlew installDist --no-daemon

FROM eclipse-temurin:25-jdk
WORKDIR /app
COPY --from=builder /app/build/install/mcp-https-kotlin/ .
ENV PORT=8080
EXPOSE 8080
CMD ["./bin/mcp-https-kotlin"]
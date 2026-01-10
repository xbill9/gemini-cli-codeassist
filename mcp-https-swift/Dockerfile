FROM swift:6.0 AS builder

WORKDIR /app
COPY . .
RUN swift build -c release

FROM swift:6.0-slim
WORKDIR /app
COPY --from=builder /app/.build/release/mcp-https-swift .
ENV PORT=8080
EXPOSE 8080
CMD ["./mcp-https-swift"]

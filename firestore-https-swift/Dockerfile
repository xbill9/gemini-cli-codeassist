FROM swift:6.2 AS builder

WORKDIR /app
COPY . .
RUN swift build -c release

FROM swift:6.2-slim
WORKDIR /app
COPY --from=builder /app/.build/release/firestore-https-swift .
ENV PORT=8080
EXPOSE 8080
CMD ["./firestore-https-swift"]

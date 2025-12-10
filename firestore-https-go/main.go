package main

import (
	"context"
	"errors"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

func getLogLevel() slog.Level {
	switch strings.ToLower(os.Getenv("LOG_LEVEL")) {
	case "debug":
		return slog.LevelDebug
	case "info":
		return slog.LevelInfo
	case "warn":
		return slog.LevelWarn
	case "error":
		return slog.LevelError
	default:
		return slog.LevelInfo
	}
}

func main() {
	// Setup Logging
	logLevel := getLogLevel()
	slog.SetDefault(slog.New(slog.NewJSONHandler(os.Stderr, &slog.HandlerOptions{
		Level: logLevel,
	})))

	// Setup Firestore
	projectID := os.Getenv("PROJECT_ID")
	if projectID == "" {
		slog.Error("PROJECT_ID must be set")
		os.Exit(1)
	}

	ctx := context.Background()
	var err error
	firestoreStore, err := NewFirestoreStore(ctx, projectID)
	if err != nil {
		slog.Error("Failed to create Firestore client", "error", err)
		os.Exit(1)
	}
	defer firestoreStore.Close()
	slog.Info("🍏 Cymbal Superstore: Inventory API Starting")

	// Create Handler Server
	srv := NewServer(firestoreStore)

	// Create MCP Server
	server := mcp.NewServer(&mcp.Implementation{
		Name:    "inventory",
		Version: "v1.0.0",
	}, nil)

	// Register Tools
	mcp.AddTool(server, &mcp.Tool{Name: "root", Description: "Inventory API via Model Context Protocol"}, srv.Root)
	mcp.AddTool(server, &mcp.Tool{Name: "health", Description: "Inventory API Health Status of Inventory API"}, srv.Health)
	mcp.AddTool(server, &mcp.Tool{Name: "echo", Description: "Inventory API via Model Context Protocol"}, srv.Echo)
	mcp.AddTool(server, &mcp.Tool{Name: "seed", Description: "Seeds the database with initial product data."}, srv.Seed)
	mcp.AddTool(server, &mcp.Tool{Name: "get_products", Description: "Retrieves a list of all products."}, srv.GetProducts)
	mcp.AddTool(server, &mcp.Tool{Name: "get_product_by_id", Description: "Retrieves a product by its ID."}, srv.GetProductByID)
	mcp.AddTool(server, &mcp.Tool{Name: "add_product", Description: "Adds a product to the database."}, srv.AddProduct)
	mcp.AddTool(server, &mcp.Tool{Name: "find", Description: "Search for products by name (case-insensitive)."}, srv.Find)
	mcp.AddTool(server, &mcp.Tool{Name: "reset", Description: "Clears the database."}, srv.Reset)

	// Create a new ServeMux to avoid using the default global mux.
	mux := http.NewServeMux()

	handler := mcp.NewStreamableHTTPHandler(
		func(*http.Request) *mcp.Server { return server },
		&mcp.StreamableHTTPOptions{},
	)
	mux.HandleFunc("/", handler.ServeHTTP)

	// Add a health check endpoint.
	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	})

	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}

	// Configure the HTTP server with timeouts for production safety.
	httpServer := &http.Server{
		Addr:              ":" + port,
		Handler:           mux,
		ReadHeaderTimeout: 5 * time.Second,
		ReadTimeout:       15 * time.Second,
		WriteTimeout:      15 * time.Second,
		IdleTimeout:       60 * time.Second,
	}

	// Run the server in a goroutine so that it doesn't block.
	go func() {
		slog.Info("Starting HTTP server", "port", port)
		if err := httpServer.ListenAndServe(); err != nil && !errors.Is(err, http.ErrServerClosed) {
			slog.Error("HTTP Server start failed", "error", err)
			os.Exit(1)
		}
	}()

	// Handle graceful shutdown
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	// Run the server
	if err := server.Run(ctx, &mcp.StdioTransport{}); err != nil {
		slog.Error("Server exit", "error", err)
		os.Exit(1)
	}

	shutdownCtx, shutdownCancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer shutdownCancel()

	if err := httpServer.Shutdown(shutdownCtx); err != nil {
		slog.Error("HTTP Server forced to shutdown", "error", err)
		os.Exit(1)
	}

	slog.Info("🍏 Cymbal Superstore: Inventory API completed")
}
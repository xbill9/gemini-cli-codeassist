package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"math/rand"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"cloud.google.com/go/firestore"
	"github.com/google/uuid"
	"github.com/modelcontextprotocol/go-sdk/mcp"
	"google.golang.org/api/iterator"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// -- Models --

type Product struct {
	ID              string    `json:"id,omitempty" firestore:"id,omitempty" jsonschema:"The ID of the product, leave empty when adding new product"`
	Name            string    `json:"name" firestore:"name" jsonschema:"The name of the product"`
	Price           float64   `json:"price" firestore:"price" jsonschema:"The price of the product"`
	Quantity        int64     `json:"quantity" firestore:"quantity" jsonschema:"The quantity available"`
	ImgFile         string    `json:"imgfile" firestore:"imgfile" jsonschema:"Image file path"`
	Timestamp       time.Time `json:"timestamp" firestore:"timestamp" jsonschema:"Record timestamp"`
	ActualDateAdded time.Time `json:"actualdateadded" firestore:"actualdateadded" jsonschema:"Actual date added"`
}

type ProductList struct {
	Products []Product `json:"products"`
}

// -- Inputs --

type EmptyInput struct{}

type EchoInput struct {
	Message string `json:"message" jsonschema:"hello world"`
}

type GetProductByIDInput struct {
	ID string `json:"id" jsonschema:"The ID of the product to retrieve"`
}

type AddProductInput struct {
	Product Product `json:"product" jsonschema:"The product to add"`
}

type SearchProductsInput struct {
	Query string `json:"query" jsonschema:"The search query to filter products by name"`
}

// -- Repository --

// InventoryStore defines the interface for data access
type InventoryStore interface {
	GetProducts(ctx context.Context) ([]Product, error)
	GetProductByID(ctx context.Context, id string) (Product, error)
	AddProduct(ctx context.Context, p Product) (string, error)
	UpsertProductByName(ctx context.Context, p Product) error
	FindProducts(ctx context.Context, query string) ([]Product, error)
	Clear(ctx context.Context) error
	Close() error
}

// FirestoreStore implements InventoryStore using Google Cloud Firestore
type FirestoreStore struct {
	client *firestore.Client
}

func NewFirestoreStore(ctx context.Context, projectID string) (*FirestoreStore, error) {
	client, err := firestore.NewClient(ctx, projectID)
	if err != nil {
		return nil, err
	}
	return &FirestoreStore{client: client}, nil
}

func (s *FirestoreStore) Close() error {
	return s.client.Close()
}

func (s *FirestoreStore) FindProducts(ctx context.Context, query string) ([]Product, error) {
	allProducts, err := s.GetProducts(ctx)
	if err != nil {
		return nil, err
	}

	var results []Product
	query = strings.ToLower(query)
	for _, p := range allProducts {
		if strings.Contains(strings.ToLower(p.Name), query) {
			results = append(results, p)
		}
	}
	return results, nil
}

func (s *FirestoreStore) GetProducts(ctx context.Context) ([]Product, error) {
	iter := s.client.Collection("inventory").Documents(ctx)
	var products []Product
	for {
		doc, err := iter.Next()
		if err == iterator.Done {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("failed to retrieve products: %w", err)
		}
		var p Product
		if err := doc.DataTo(&p); err != nil {
			slog.Error("Failed to parse product data", "id", doc.Ref.ID, "error", err)
			continue // Skip malformed
		}
		p.ID = doc.Ref.ID
		products = append(products, p)
	}
	return products, nil
}

func (s *FirestoreStore) GetProductByID(ctx context.Context, id string) (Product, error) {
	docRef := s.client.Collection("inventory").Doc(id)
	doc, err := docRef.Get(ctx)
	if err != nil {
		if status.Code(err) == codes.NotFound {
			return Product{}, fmt.Errorf("product with ID %s not found", id)
		}
		return Product{}, fmt.Errorf("failed to retrieve product: %w", err)
	}

	var p Product
	if err := doc.DataTo(&p); err != nil {
		return Product{}, fmt.Errorf("failed to parse product: %w", err)
	}
	p.ID = doc.Ref.ID
	return p, nil
}

func (s *FirestoreStore) AddProduct(ctx context.Context, p Product) (string, error) {
	newID := uuid.New().String()
	_, err := s.client.Collection("inventory").Doc(newID).Set(ctx, p)
	if err != nil {
		slog.Error("Firestore: Failed to add product", "product_name", p.Name, "error", err)
		return "", fmt.Errorf("failed to add product: %w", err)
	}
	return newID, nil
}

func (s *FirestoreStore) UpsertProductByName(ctx context.Context, p Product) error {
	// Note: This operation is not atomic and has a race condition.
	// Ideally, we should use a transaction, but querying within a transaction
	// requires ensuring the client SDK supports `tx.Documents(q)`.
	// For now, we accept the race condition for simplicity in this demo.
	iter := s.client.Collection("inventory").Where("name", "==", p.Name).Limit(1).Documents(ctx)
	doc, err := iter.Next()

	if err == iterator.Done {
		// Insert new
		newID := uuid.New().String()
		_, err := s.client.Collection("inventory").Doc(newID).Set(ctx, p)
		if err != nil {
			slog.Error("Firestore: Failed to insert new product during upsert", "product_name", p.Name, "error", err)
		}
		return err
	}
	if err != nil {
		slog.Error("Firestore: Failed to query product during upsert", "product_name", p.Name, "error", err)
		return err
	}

	// Update existing
	_, err = doc.Ref.Set(ctx, p) // Set overwrites.
	if err != nil {
		slog.Error("Firestore: Failed to update existing product during upsert", "product_name", p.Name, "error", err)
	}
	return err
}

func (s *FirestoreStore) Clear(ctx context.Context) error {
	iter := s.client.Collection("inventory").Documents(ctx)
	bw := s.client.BulkWriter(ctx)

	for {
		doc, err := iter.Next()
		if err == iterator.Done {
			break
		}
		if err != nil {
			slog.Error("Firestore: Failed to iterate documents during clear", "error", err)
			return fmt.Errorf("failed to iterate documents: %w", err)
		}
		_, err = bw.Delete(doc.Ref)
		if err != nil {
			slog.Error("Firestore: Failed to delete document during clear", "doc_id", doc.Ref.ID, "error", err)
			return fmt.Errorf("failed to delete document: %w", err)
		}
	}
	bw.Flush()
	return nil
}

// -- Handlers --

// Server holds the dependencies for the application
type Server struct {
	Store InventoryStore
}

// NewServer creates a new Server instance
func NewServer(store InventoryStore) *Server {
	return &Server{
		Store: store,
	}
}

// -- Helper to return text result --
func textResult(msg string) *mcp.CallToolResult {
	return &mcp.CallToolResult{
		Content: []mcp.Content{
			&mcp.TextContent{
				Text: msg,
			},
		},
	}
}

// -- Tool Implementations --

func (s *Server) Root(ctx context.Context, req *mcp.CallToolRequest, input EmptyInput) (*mcp.CallToolResult, EmptyInput, error) {
	return textResult("🍎 Hello! This is the Cymbal Superstore Inventory API."), EmptyInput{}, nil
}

func (s *Server) Health(ctx context.Context, req *mcp.CallToolRequest, input EmptyInput) (*mcp.CallToolResult, EmptyInput, error) {
	return textResult("✅ ok"), EmptyInput{}, nil
}

func (s *Server) Echo(ctx context.Context, req *mcp.CallToolRequest, input EchoInput) (*mcp.CallToolResult, EmptyInput, error) {
	msg := fmt.Sprintf("Inventory MCP! %s", input.Message)
	return textResult(msg), EmptyInput{}, nil
}

func (s *Server) Seed(ctx context.Context, req *mcp.CallToolRequest, input EmptyInput) (*mcp.CallToolResult, EmptyInput, error) {
	slog.Info("Seeding database...")

	oldProducts := generateProducts([]string{
		"Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
		"Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
		"Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli", "Jasmine Rice",
		"Yogurt", "Beef", "Shrimp", "Walnuts", "Sunflower Seeds", "Fresh Basil", "Cinnamon",
	}, 1, 501, 90, 365)

	for _, p := range oldProducts {
		if err := s.Store.UpsertProductByName(ctx, p); err != nil {
			slog.Error("Error adding/updating product", "error", err)
			return nil, EmptyInput{}, fmt.Errorf("failed to add/update product: %w", err)
		}
	}

	recentProducts := generateProducts([]string{
		"Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter", "Mint Chocolate Cookies",
		"White Chocolate Caramel Corn", "Acai Smoothie Packs", "Smores Cereal",
		"Peanut Butter and Jelly Cups",
	}, 1, 101, 0, 6)

	for _, p := range recentProducts {
		if err := s.Store.UpsertProductByName(ctx, p); err != nil {
			slog.Error("Error adding/updating product", "error", err)
			return nil, EmptyInput{}, fmt.Errorf("failed to add/update product: %w", err)
		}
	}

	oosProducts := generateProducts([]string{
		"Wasabi Party Mix", "Jalapeno Seasoning",
	}, 0, 1, 0, 6)

	for _, p := range oosProducts {
		if err := s.Store.UpsertProductByName(ctx, p); err != nil {
			slog.Error("Error adding/updating product", "error", err)
			return nil, EmptyInput{}, fmt.Errorf("failed to add/update product: %w", err)
		}
	}

	return textResult("Database seeded successfully."), EmptyInput{}, nil
}

func (s *Server) GetProducts(ctx context.Context, req *mcp.CallToolRequest, input EmptyInput) (*mcp.CallToolResult, EmptyInput, error) {
	products, err := s.Store.GetProducts(ctx)
	if err != nil {
		slog.Error("Failed to retrieve products from store", "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to retrieve products: %w", err)
	}

	productList := ProductList{Products: products}
	jsonBytes, err := json.Marshal(productList)
	if err != nil {
		slog.Error("Failed to serialize product list", "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to serialize product list: %w", err)
	}

	return textResult(string(jsonBytes)), EmptyInput{}, nil
}

func (s *Server) GetProductByID(ctx context.Context, req *mcp.CallToolRequest, input GetProductByIDInput) (*mcp.CallToolResult, EmptyInput, error) {
	p, err := s.Store.GetProductByID(ctx, input.ID)
	if err != nil {
		slog.Error("Failed to retrieve product by ID from store", "id", input.ID, "error", err)
		return nil, EmptyInput{}, err
	}

	jsonBytes, err := json.Marshal(p)
	if err != nil {
		slog.Error("Failed to serialize product", "id", input.ID, "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to serialize product: %w", err)
	}

	return textResult(string(jsonBytes)), EmptyInput{}, nil
}

func (s *Server) AddProduct(ctx context.Context, req *mcp.CallToolRequest, input AddProductInput) (*mcp.CallToolResult, EmptyInput, error) {
	p := input.Product
	newID, err := s.Store.AddProduct(ctx, p)
	if err != nil {
		slog.Error("Failed to add product to store", "product_name", p.Name, "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to add product: %w", err)
	}

	return textResult(fmt.Sprintf("Product added with ID: %s", newID)), EmptyInput{}, nil
}

func (s *Server) Find(ctx context.Context, req *mcp.CallToolRequest, input SearchProductsInput) (*mcp.CallToolResult, EmptyInput, error) {
	products, err := s.Store.FindProducts(ctx, input.Query)
	if err != nil {
		slog.Error("Failed to find products from store", "query", input.Query, "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to search products: %w", err)
	}

	productList := ProductList{Products: products}
	jsonBytes, err := json.Marshal(productList)
	if err != nil {
		slog.Error("Failed to serialize product list", "query", input.Query, "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to serialize product list: %w", err)
	}

	return textResult(string(jsonBytes)), EmptyInput{}, nil
}

func (s *Server) Reset(ctx context.Context, req *mcp.CallToolRequest, input EmptyInput) (*mcp.CallToolResult, EmptyInput, error) {
	if err := s.Store.Clear(ctx); err != nil {
		slog.Error("Failed to clear database", "error", err)
		return nil, EmptyInput{}, fmt.Errorf("failed to clear database: %w", err)
	}
	return textResult("Database cleared successfully."), EmptyInput{}, nil
}

// -- Helpers --

func generateProducts(names []string, minQ, maxQ int64, minDays, maxDays int) []Product {
	var products []Product
	rnd := rand.New(rand.NewSource(time.Now().UnixNano()))

	for _, name := range names {
		daysAgo := rnd.Intn(maxDays-minDays) + minDays
		timestamp := time.Now().AddDate(0, 0, -daysAgo)

		p := Product{
			Name:            name,
			Price:           float64(rnd.Intn(10)) + 1.0 + rnd.Float64(), // 1.0 to 11.0 roughly
			Quantity:        int64(rnd.Intn(int(maxQ-minQ))) + minQ,
			ImgFile:         fmt.Sprintf("product-images/%s.png", strings.ToLower(strings.ReplaceAll(name, " ", ""))),
			Timestamp:       timestamp,
			ActualDateAdded: time.Now(),
		}
		products = append(products, p)
	}
	return products
}

// -- Main --

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

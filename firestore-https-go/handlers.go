package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"math/rand"
	"strings"
	"time"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

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

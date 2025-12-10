package main

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"testing"

	"github.com/google/uuid"
	"github.com/modelcontextprotocol/go-sdk/mcp"
)

// MockInventoryStore implements InventoryStore for testing
type MockInventoryStore struct {
	Products map[string]Product
	Err      error
}

func NewMockInventoryStore() *MockInventoryStore {
	return &MockInventoryStore{
		Products: make(map[string]Product),
	}
}

func (m *MockInventoryStore) GetProducts(ctx context.Context) ([]Product, error) {
	if m.Err != nil {
		return nil, m.Err
	}
	var products []Product
	for _, p := range m.Products {
		products = append(products, p)
	}
	return products, nil
}

func (m *MockInventoryStore) GetProductByID(ctx context.Context, id string) (Product, error) {
	if m.Err != nil {
		return Product{}, m.Err
	}
	p, ok := m.Products[id]
	if !ok {
		return Product{}, fmt.Errorf("product with ID %s not found", id)
	}
	return p, nil
}

func (m *MockInventoryStore) AddProduct(ctx context.Context, p Product) (string, error) {
	if m.Err != nil {
		return "", m.Err
	}
	id := uuid.New().String()
	p.ID = id
	m.Products[id] = p
	return id, nil
}

func (m *MockInventoryStore) UpsertProductByName(ctx context.Context, p Product) error {
	if m.Err != nil {
		return m.Err
	}
	for id, existing := range m.Products {
		if existing.Name == p.Name {
			p.ID = id
			m.Products[id] = p
			return nil
		}
	}
	id := uuid.New().String()
	p.ID = id
	m.Products[id] = p
	return nil
}

func (m *MockInventoryStore) Close() error {
	return nil
}

// Helper to reset store
func setupStore() *MockInventoryStore {
	mock := NewMockInventoryStore()
	store = mock
	return mock
}

func TestRoot(t *testing.T) {
	ctx := context.Background()
	res, _, err := Root(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("Root returned error: %v", err)
	}
	content := res.Content[0].(*mcp.TextContent).Text
	if !strings.Contains(content, "Hello") {
		t.Errorf("Expected greeting, got: %s", content)
	}
}

func TestHealth(t *testing.T) {
	ctx := context.Background()
	res, _, err := Health(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("Health returned error: %v", err)
	}
	content := res.Content[0].(*mcp.TextContent).Text
	if content != "✅ ok" {
		t.Errorf("Expected '✅ ok', got: %s", content)
	}
}

func TestEcho(t *testing.T) {
	ctx := context.Background()
	input := EchoInput{Message: "Test"}
	res, _, err := Echo(ctx, nil, input)
	if err != nil {
		t.Fatalf("Echo returned error: %v", err)
	}
	content := res.Content[0].(*mcp.TextContent).Text
	if !strings.Contains(content, "Test") {
		t.Errorf("Expected echo to contain 'Test', got: %s", content)
	}
}

func TestGenerateProducts(t *testing.T) {
	names := []string{"TestProd1", "TestProd2"}
	products := generateProducts(names, 1, 10, 0, 1)

	if len(products) != 2 {
		t.Errorf("Expected 2 products, got %d", len(products))
	}
	if products[0].Name != "TestProd1" {
		t.Errorf("Expected TestProd1, got %s", products[0].Name)
	}
}

func TestAddProduct(t *testing.T) {
	setupStore()
	ctx := context.Background()
	input := AddProductInput{
		Product: Product{Name: "New Product", Price: 10.0},
	}

	res, _, err := AddProduct(ctx, nil, input)
	if err != nil {
		t.Fatalf("AddProduct failed: %v", err)
	}

	content := res.Content[0].(*mcp.TextContent).Text
	if !strings.Contains(content, "Product added with ID:") {
		t.Errorf("Expected success message, got: %s", content)
	}
}

func TestGetProductByID(t *testing.T) {
	mock := setupStore()
	ctx := context.Background()

	// Add a product manually to mock
	id := "test-id-123"
	mock.Products[id] = Product{ID: id, Name: "Existing Product", Price: 5.0}

	input := GetProductByIDInput{ID: id}
	res, _, err := GetProductByID(ctx, nil, input)
	if err != nil {
		t.Fatalf("GetProductByID failed: %v", err)
	}

	var p Product
	content := res.Content[0].(*mcp.TextContent).Text
	if err := json.Unmarshal([]byte(content), &p); err != nil {
		t.Fatalf("Failed to unmarshal result: %v", err)
	}

	if p.Name != "Existing Product" {
		t.Errorf("Expected product name 'Existing Product', got %s", p.Name)
	}
}

func TestGetProducts(t *testing.T) {
	mock := setupStore()
	ctx := context.Background()

	mock.Products["1"] = Product{ID: "1", Name: "P1"}
	mock.Products["2"] = Product{ID: "2", Name: "P2"}

	res, _, err := GetProducts(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("GetProducts failed: %v", err)
	}

	var list ProductList
	content := res.Content[0].(*mcp.TextContent).Text
	if err := json.Unmarshal([]byte(content), &list); err != nil {
		t.Fatalf("Failed to unmarshal result: %v", err)
	}

	if len(list.Products) != 2 {
		t.Errorf("Expected 2 products, got %d", len(list.Products))
	}
}

func TestSeed(t *testing.T) {
	mock := setupStore()
	ctx := context.Background()

	res, _, err := Seed(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("Seed failed: %v", err)
	}

	if len(mock.Products) == 0 {
		t.Error("Seed did not add any products")
	}

	content := res.Content[0].(*mcp.TextContent).Text
	if content != "Database seeded successfully." {
		t.Errorf("Expected success message, got: %s", content)
	}
}

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/google/uuid"
	"github.com/modelcontextprotocol/go-sdk/mcp"
)

// MockInventoryStore implements InventoryStore for testing
type MockInventoryStore struct {
	mu       sync.RWMutex
	Products map[string]Product
	Err      error
}

func NewMockInventoryStore() *MockInventoryStore {
	return &MockInventoryStore{
		Products: make(map[string]Product),
	}
}

func (m *MockInventoryStore) GetProducts(ctx context.Context) ([]Product, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

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
	m.mu.RLock()
	defer m.mu.RUnlock()

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
	m.mu.Lock()
	defer m.mu.Unlock()

	if m.Err != nil {
		return "", m.Err
	}
	id := uuid.New().String()
	p.ID = id
	m.Products[id] = p
	return id, nil
}

func (m *MockInventoryStore) UpsertProductByName(ctx context.Context, p Product) error {
	m.mu.Lock()
	defer m.mu.Unlock()

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

func (m *MockInventoryStore) FindProducts(ctx context.Context, query string) ([]Product, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	if m.Err != nil {
		return nil, m.Err
	}
	var results []Product
	query = strings.ToLower(query)
	for _, p := range m.Products {
		if strings.Contains(strings.ToLower(p.Name), query) {
			results = append(results, p)
		}
	}
	return results, nil
}

func (m *MockInventoryStore) Close() error {
	return nil
}

func (m *MockInventoryStore) Clear(ctx context.Context) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	if m.Err != nil {
		return m.Err
	}
	m.Products = make(map[string]Product)
	return nil
}

// Helper to create a server with a mock store
func setupServer() (*Server, *MockInventoryStore) {
	mock := NewMockInventoryStore()
	srv := NewServer(mock)
	return srv, mock
}

func TestRoot(t *testing.T) {
	srv, _ := setupServer()
	ctx := context.Background()
	res, _, err := srv.Root(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("Root returned error: %v", err)
	}
	content := res.Content[0].(*mcp.TextContent).Text
	if !strings.Contains(content, "Hello") {
		t.Errorf("Expected greeting, got: %s", content)
	}
}

func TestHealth(t *testing.T) {
	srv, _ := setupServer()
	ctx := context.Background()
	res, _, err := srv.Health(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("Health returned error: %v", err)
	}
	content := res.Content[0].(*mcp.TextContent).Text
	if content != "✅ ok" {
		t.Errorf("Expected '✅ ok', got: %s", content)
	}
}

func TestEcho(t *testing.T) {
	srv, _ := setupServer()
	ctx := context.Background()
	input := EchoInput{Message: "Test"}
	res, _, err := srv.Echo(ctx, nil, input)
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
	srv, _ := setupServer()
	ctx := context.Background()
	input := AddProductInput{
		Product: Product{Name: "New Product", Price: 10.0},
	}

	res, _, err := srv.AddProduct(ctx, nil, input)
	if err != nil {
		t.Fatalf("AddProduct failed: %v", err)
	}

	content := res.Content[0].(*mcp.TextContent).Text
	if !strings.Contains(content, "Product added with ID:") {
		t.Errorf("Expected success message, got: %s", content)
	}
}

func TestGetProductByID(t *testing.T) {
	srv, mock := setupServer()
	ctx := context.Background()

	// Add a product manually to mock
	id := "test-id-123"
	mock.Products[id] = Product{ID: id, Name: "Existing Product", Price: 5.0}

	input := GetProductByIDInput{ID: id}
	res, _, err := srv.GetProductByID(ctx, nil, input)
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
	srv, mock := setupServer()
	ctx := context.Background()

	mock.Products["1"] = Product{ID: "1", Name: "P1"}
	mock.Products["2"] = Product{ID: "2", Name: "P2"}

	res, _, err := srv.GetProducts(ctx, nil, EmptyInput{})
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

func TestFind(t *testing.T) {
	srv, mock := setupServer()
	ctx := context.Background()

	mock.Products["1"] = Product{ID: "1", Name: "Apple Pie"}
	mock.Products["2"] = Product{ID: "2", Name: "Green Apple"}
	mock.Products["3"] = Product{ID: "3", Name: "Banana"}

	input := SearchProductsInput{Query: "Apple"}
	res, _, err := srv.Find(ctx, nil, input)
	if err != nil {
		t.Fatalf("SearchProducts failed: %v", err)
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
	srv, mock := setupServer()
	ctx := context.Background()

	res, _, err := srv.Seed(ctx, nil, EmptyInput{})
	if err != nil {
		t.Fatalf("Seed failed: %v", err)
	}

	mock.mu.RLock()
	count := len(mock.Products)
	mock.mu.RUnlock()

	if count == 0 {
		t.Error("Seed did not add any products")
	}

	content := res.Content[0].(*mcp.TextContent).Text
		if content != "Database seeded successfully." {
			t.Errorf("Expected success message, got: %s", content)
		}
	}
	
	func TestReset(t *testing.T) {
		srv, mock := setupServer()
		ctx := context.Background()
	
		// Add some products
		mock.Products["1"] = Product{ID: "1", Name: "P1"}
	
		res, _, err := srv.Reset(ctx, nil, EmptyInput{})
		if err != nil {
			t.Fatalf("Reset failed: %v", err)
		}
	
		content := res.Content[0].(*mcp.TextContent).Text
		if content != "Database cleared successfully." {
			t.Errorf("Expected success message, got: %s", content)
		}
	
		mock.mu.RLock()
		count := len(mock.Products)
		mock.mu.RUnlock()
	
		if count != 0 {
			t.Errorf("Expected 0 products after reset, got %d", count)
		}
	}
	
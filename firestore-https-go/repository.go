package main

import (
	"context"
	"fmt"
	"log/slog"
	"strings"

	"cloud.google.com/go/firestore"
	"github.com/google/uuid"
	"google.golang.org/api/iterator"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

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

package main

import (
	"context"
	"fmt"
	"strings"

	"cloud.google.com/go/firestore"
	"github.com/google/uuid"
	"google.golang.org/api/iterator"
)

// InventoryStore defines the interface for data access
type InventoryStore interface {
	GetProducts(ctx context.Context) ([]Product, error)
	GetProductByID(ctx context.Context, id string) (Product, error)
	AddProduct(ctx context.Context, p Product) (string, error)
	UpsertProductByName(ctx context.Context, p Product) error
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
		// Checks for "NotFound" or "not found" in error message as the error type might be wrapped
		if strings.Contains(err.Error(), "NotFound") || strings.Contains(err.Error(), "not found") {
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
		return "", fmt.Errorf("failed to add product: %w", err)
	}
	return newID, nil
}

func (s *FirestoreStore) UpsertProductByName(ctx context.Context, p Product) error {
	iter := s.client.Collection("inventory").Where("name", "==", p.Name).Limit(1).Documents(ctx)
	doc, err := iter.Next()

	if err == iterator.Done {
		// Insert new
		newID := uuid.New().String()
		_, err := s.client.Collection("inventory").Doc(newID).Set(ctx, p)
		return err
	}
	if err != nil {
		return err
	}

	// Update existing
	_, err = doc.Ref.Set(ctx, p) // Set overwrites.
	return err
}

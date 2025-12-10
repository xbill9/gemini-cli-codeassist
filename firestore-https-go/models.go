package main

import (
	"time"
)

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

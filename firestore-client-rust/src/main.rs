use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};
use chrono::{DateTime, Duration, Utc};
use firestore::FirestoreDb;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Product {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    name: String,
    price: f64,
    quantity: i64,
    imgfile: String,
    timestamp: DateTime<Utc>,
    actualdateadded: DateTime<Utc>,
}

async fn add_or_update_firestore(
    db: &Arc<FirestoreDb>,
    product: &Product,
) -> Result<(), firestore::errors::FirestoreError> {
    let existing_docs = db
        .fluent()
        .select()
        .from("inventory")
        .filter(|q| q.field("name").eq(&product.name))
        .limit(1)
        .query()
        .await?;

    if let Some(doc) = existing_docs.first() {
        let doc_id = doc.name.split('/').next_back().unwrap_or_default();
        if !doc_id.is_empty() {
            let mut product_with_id = product.clone();
            product_with_id.id = Some(doc_id.to_string());
            db.fluent()
                .update()
                .in_col("inventory")
                .document_id(doc_id)
                .object(&product_with_id)
                .execute::<()>()
                .await?;
        }
    } else {
        let product_id = Uuid::new_v4().to_string();
        let mut product_with_id = product.clone();
        product_with_id.id = Some(product_id.clone());
        db.fluent()
            .insert()
            .into("inventory")
            .document_id(&product_id)
            .object(&product_with_id)
            .execute::<()>()
            .await?;
    }
    Ok(())
}

#[axum::debug_handler]
async fn seed(State(db): State<Arc<FirestoreDb>>) -> Result<&'static str, StatusCode> {
    let old_products_to_add: Vec<Product> = {
        let mut rng = rand::rng();
        let old_products = [
            "Apples",
            "Bananas",
            "Milk",
            "Whole Wheat Bread",
            "Eggs",
            "Cheddar Cheese",
            "Whole Chicken",
            "Rice",
            "Black Beans",
            "Bottled Water",
            "Apple Juice",
            "Cola",
            "Coffee Beans",
            "Green Tea",
            "Watermelon",
            "Broccoli",
            "Jasmine Rice",
            "Yogurt",
            "Beef",
            "Shrimp",
            "Walnuts",
            "Sunflower Seeds",
            "Fresh Basil",
            "Cinnamon",
        ];
        old_products
            .iter()
            .map(|&product_name| Product {
                id: None,
                name: product_name.to_string(),
                price: rng.random_range(1.0..11.0),
                quantity: rng.random_range(1..501),
                imgfile: format!(
                    "product-images/{}.png",
                    product_name.replace(' ', "").to_lowercase()
                ),
                timestamp: Utc::now() - Duration::days(rng.random_range(90..365)),
                actualdateadded: Utc::now(),
            })
            .collect()
    };

    for product in old_products_to_add {
        if add_or_update_firestore(&db, &product).await.is_err() {
            return Err(StatusCode::INTERNAL_SERVER_ERROR);
        }
    }

    let recent_products_to_add: Vec<Product> = {
        let mut rng = rand::rng();
        let recent_products = [
            "Parmesan Crisps",
            "Pineapple Kombucha",
            "Maple Almond Butter",
            "Mint Chocolate Cookies",
            "White Chocolate Caramel Corn",
            "Acai Smoothie Packs",
            "Smores Cereal",
            "Peanut Butter and Jelly Cups",
        ];
        recent_products
            .iter()
            .map(|&product_name| Product {
                id: None,
                name: product_name.to_string(),
                price: rng.random_range(1.0..11.0),
                quantity: rng.random_range(1..101),
                imgfile: format!(
                    "product-images/{}.png",
                    product_name.replace(' ', "").to_lowercase()
                ),
                timestamp: Utc::now() - Duration::days(rng.random_range(0..6)),
                actualdateadded: Utc::now(),
            })
            .collect()
    };

    for product in recent_products_to_add {
        if add_or_update_firestore(&db, &product).await.is_err() {
            return Err(StatusCode::INTERNAL_SERVER_ERROR);
        }
    }

    let oos_products_to_add: Vec<Product> = {
        let mut rng = rand::rng();
        let recent_products_out_of_stock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
        recent_products_out_of_stock
            .iter()
            .map(|&product_name| Product {
                id: None,
                name: product_name.to_string(),
                price: rng.random_range(1.0..11.0),
                quantity: 0,
                imgfile: format!(
                    "product-images/{}.png",
                    product_name.replace(' ', "").to_lowercase()
                ),
                timestamp: Utc::now() - Duration::days(rng.random_range(0..6)),
                actualdateadded: Utc::now(),
            })
            .collect()
    };

    for product in oos_products_to_add {
        if add_or_update_firestore(&db, &product).await.is_err() {
            return Err(StatusCode::INTERNAL_SERVER_ERROR);
        }
    }

    Ok("Database seeded successfully.")
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let _ =
        rustls::crypto::CryptoProvider::install_default(rustls::crypto::ring::default_provider());
    let gcp_project_id = std::env::var("PROJECT_ID").expect("PROJECT_ID must be set");
    let db = Arc::new(FirestoreDb::new(&gcp_project_id).await?);

    println!("Seeding database...");
    if let Err(e) = seed(State(db.clone())).await {
        let err_msg = format!("Error seeding database: {}", e);
        eprintln!("{}", err_msg);
        return Err(err_msg.into());
    }

    println!(
        "
Calling health check..."
    );
    let health_status = health().await;
    println!("Health status: {}", health_status);

    println!(
        "
Calling root..."
    );
    let root_status = root().await;
    println!("Root status: {}", root_status);

    println!(
        "
Listing all products..."
    );
    let products = match get_products(State(db.clone())).await {
        Ok(Json(products)) => {
            println!("Found {} products.", products.len());
            products
        }
        Err(e) => {
            let err_msg = format!("Error listing products: {}", e);
            eprintln!("{}", err_msg);
            return Err(err_msg.into());
        }
    };

    println!(
        "
Finding product 'Coffee Beans'..."
    );
    if let Some(coffee_product) = products.iter().find(|p| p.name == "Coffee Beans") {
        if let Some(id) = &coffee_product.id {
            println!("Found 'Coffee Beans' with id: {}", id);
            println!("Fetching product by id...");
            match get_product_by_id(State(db.clone()), Path(id.clone())).await {
                Ok(Json(product)) => println!("Found product: {:?}", product),
                Err(StatusCode::NOT_FOUND) => println!("Product with id {} not found.", id),
                Err(e) => {
                    let err_msg = format!("Error getting product by id: {}", e);
                    eprintln!("{}", err_msg);
                    return Err(err_msg.into());
                }
            }
        } else {
            println!("'Coffee Beans' product found, but it has no ID.");
        }
    } else {
        println!("Product 'Coffee Beans' not found in the list of products.");
    }

    Ok(())
}

async fn root() -> &'static str {
    "🍎 Hello! This is the Cymbal Superstore Inventory API."
}

async fn health() -> &'static str {
    "✅ ok"
}

async fn get_products(
    State(db): State<Arc<FirestoreDb>>,
) -> Result<Json<Vec<Product>>, StatusCode> {
    let products: Vec<Product> = db
        .fluent()
        .select()
        .from("inventory")
        .obj()
        .query()
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok(Json(products))
}

async fn get_product_by_id(
    State(db): State<Arc<FirestoreDb>>,
    Path(id): Path<String>,
) -> Result<Json<Product>, StatusCode> {
    let product: Option<Product> = db
        .fluent()
        .select()
        .by_id_in("inventory")
        .obj()
        .one(&id)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    if let Some(product) = product {
        Ok(Json(product))
    } else {
        Err(StatusCode::NOT_FOUND)
    }
}

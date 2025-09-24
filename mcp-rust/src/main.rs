use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::Json,
    routing::get,
    Router,
};
use chrono::{DateTime, Utc};
use firestore::FirestoreDb;
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

#[derive(Debug, Deserialize)]
struct DocName {
    name: String,
}

use chrono::Duration;
use rand::Rng;

async fn add_or_update_firestore(
    db: &Arc<FirestoreDb>,
    product: &Product,
) -> Result<(), firestore::errors::FirestoreError> {
    let existing_docs: Vec<DocName> = db
        .fluent()
        .select()
        .from("inventory")
        .filter(|q| q.field("name").eq(&product.name))
        .limit(1)
        .obj()
        .query()
        .await?;

    if let Some(doc) = existing_docs.first() {
        let doc_id = doc.name.split('/').next_back().unwrap_or_default();
        if !doc_id.is_empty() {
            db.fluent()
                .update()
                .in_col("inventory")
                .document_id(doc_id)
                .object(product)
                .execute::<()>()
                .await?;
        }
    } else {
        let product_id = Uuid::new_v4().to_string();
        db.fluent()
            .insert()
            .into("inventory")
            .document_id(&product_id)
            .object(product)
            .execute::<()>()
            .await?;
    }
    Ok(())
}

#[axum::debug_handler]
async fn seed(State(db): State<Arc<FirestoreDb>>) -> Result<&'static str, StatusCode> {
    let old_products_to_add: Vec<Product> = {
        let mut rng = rand::thread_rng();
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
                price: rng.gen_range(1.0..11.0),
                quantity: rng.gen_range(1..501),
                imgfile: format!(
                    "product-images/{}.png",
                    product_name.replace(' ', "").to_lowercase()
                ),
                timestamp: Utc::now() - Duration::days(rng.gen_range(90..365)),
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
        let mut rng = rand::thread_rng();
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
                price: rng.gen_range(1.0..11.0),
                quantity: rng.gen_range(1..101),
                imgfile: format!(
                    "product-images/{}.png",
                    product_name.replace(' ', "").to_lowercase()
                ),
                timestamp: Utc::now() - Duration::days(rng.gen_range(0..6)),
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
        let mut rng = rand::thread_rng();
        let recent_products_out_of_stock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
        recent_products_out_of_stock
            .iter()
            .map(|&product_name| Product {
                id: None,
                name: product_name.to_string(),
                price: rng.gen_range(1.0..11.0),
                quantity: 0,
                imgfile: format!(
                    "product-images/{}.png",
                    product_name.replace(' ', "").to_lowercase()
                ),
                timestamp: Utc::now() - Duration::days(rng.gen_range(0..6)),
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
    dotenv::dotenv().ok();

    let gcp_project_id = std::env::var("PROJECT_ID").expect("PROJECT_ID must be set");
    let db = Arc::new(FirestoreDb::new(&gcp_project_id).await?);

    let app = Router::new()
        .route("/", get(root))
        .route("/health", get(health))
        .route("/products", get(get_products))
        .route("/products/{id}", get(get_product_by_id))
        .route("/seed", get(seed))
        .with_state(db);

    let port = std::env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{}", port)).await?;
    println!(
        "🍏 Cymbal Superstore: Inventory API running on port: {}",
        port
    );
    axum::serve(listener, app).await?;

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

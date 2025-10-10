use anyhow::{Context, Result};
use chrono::{DateTime, Duration, Utc};
use clap::Parser;
use firestore::FirestoreDb;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::info;
use tracing_subscriber::{fmt, prelude::*, filter::EnvFilter};
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

async fn seed(db: &Arc<FirestoreDb>) -> anyhow::Result<&'static str> {
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
        add_or_update_firestore(db, &product).await?;
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
        add_or_update_firestore(db, &product).await?;
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
        add_or_update_firestore(db, &product).await?;
    }

    Ok("Database seeded successfully.")
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Parser, Debug)]
enum Commands {
    /// Seeds the Firestore database with initial product data
    Seed,
    /// Lists all products in the Firestore database
    List,
    /// Gets a product by its ID from the Firestore database
    Get {
        /// The ID of the product to retrieve
        id: String,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();

    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();

    let _ =
        rustls::crypto::CryptoProvider::install_default(rustls::crypto::ring::default_provider());

    let cli = Cli::parse();

    let gcp_project_id = std::env::var("PROJECT_ID")
        .context("PROJECT_ID environment variable not set")?;
    let db = Arc::new(FirestoreDb::new(&gcp_project_id).await?);

    match cli.command {
        Commands::Seed => {
            info!("Seeding database...");
            seed(&db).await?;
            info!("Database seeded successfully.");
        }
        Commands::List => {
            info!("Listing all products...");
            let products = get_products(&db).await?;
            for product in products {
                info!("{:?}", product);
            }
        }
        Commands::Get { id } => {
            info!("Fetching product by id: {}", id);
            match get_product_by_id(&db, id.clone()).await? {
                Some(product) => info!("Found product: {:?}", product),
                None => info!("Product with id {} not found.", id),
            }
        }
    }

    Ok(())
}

async fn get_products(
    db: &Arc<FirestoreDb>,
) -> anyhow::Result<Vec<Product>> {
    let products: Vec<Product> = db
        .fluent()
        .select()
        .from("inventory")
        .obj()
        .query()
        .await
        .context("Failed to query products from Firestore")?;

    Ok(products)
}

async fn get_product_by_id(
    db: &Arc<FirestoreDb>,
    id: String,
) -> anyhow::Result<Option<Product>> {
    let product: Option<Product> = db
        .fluent()
        .select()
        .by_id_in("inventory")
        .obj()
        .one(&id)
        .await
        .context("Failed to query product by ID from Firestore")?;

    Ok(product)
}

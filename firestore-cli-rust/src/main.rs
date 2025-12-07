use anyhow::{Context, Result};
use chrono::{DateTime, Duration, Utc};
use clap::Parser;
use firestore::FirestoreDb;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::info;
use tracing_subscriber::{filter::EnvFilter, fmt, prelude::*};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
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

    let mut product_to_write = product.clone();

    if let Some(doc) = existing_docs.first() {
        let doc_id = doc.name.split('/').next_back().unwrap_or_default(); // Reverted to original method
        if !doc_id.is_empty() {
            product_to_write.id = Some(doc_id.to_string());
            db.fluent()
                .update()
                .in_col("inventory")
                .document_id(doc_id)
                .object(&product_to_write)
                .execute::<()>()
                .await?;
        }
    } else {
        let product_id = Uuid::new_v4().to_string();
        product_to_write.id = Some(product_id.clone());
        db.fluent()
            .insert()
            .into("inventory")
            .document_id(&product_id)
            .object(&product_to_write)
            .execute::<()>()
            .await?;
    }
    Ok(())
}

fn generate_products(
    product_names: &[&str],
    price_range: (f64, f64),
    quantity_range: (i64, i64),
    days_ago_range: (i64, i64),
    fixed_quantity: Option<i64>,
) -> Vec<Product> {
    let mut rng = rand::thread_rng();
    product_names
        .iter()
        .map(|&product_name| Product {
            id: None,
            name: product_name.to_string(),
            price: rng.gen_range(price_range.0..price_range.1),
            quantity: fixed_quantity
                .unwrap_or_else(|| rng.gen_range(quantity_range.0..quantity_range.1)),
            imgfile: format!(
                "product-images/{}.png",
                product_name.replace(' ', "").to_lowercase()
            ),
            timestamp: Utc::now()
                - Duration::days(rng.gen_range(days_ago_range.0..days_ago_range.1)),
            actualdateadded: Utc::now(),
        })
        .collect()
}

async fn seed(db: &Arc<FirestoreDb>) -> anyhow::Result<&'static str> {
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
    let old_products_to_add =
        generate_products(&old_products, (1.0, 11.0), (1, 501), (90, 365), None);

    for product in old_products_to_add {
        add_or_update_firestore(db, &product).await?;
    }

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
    let recent_products_to_add =
        generate_products(&recent_products, (1.0, 11.0), (1, 101), (0, 6), None);

    for product in recent_products_to_add {
        add_or_update_firestore(db, &product).await?;
    }

    let recent_products_out_of_stock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
    let oos_products_to_add = generate_products(
        &recent_products_out_of_stock,
        (1.0, 11.0),
        (0, 0),
        (0, 6),
        Some(0),
    );

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
    /// Deletes a product by its ID from the Firestore database
    Delete {
        /// The ID of the product to delete
        id: String,
    },
    /// Finds products by name (case-insensitive, partial match)
    Find {
        /// The name or partial name of the product to find
        name: String,
    },
    /// Adds a new product to the Firestore database
    Add {
        /// The name of the product
        #[arg(long)]
        name: String,
        /// The price of the product
        #[arg(long)]
        price: f64,
        /// The quantity of the product
        #[arg(long)]
        quantity: i64,
        /// The image file path/name of the product
        #[arg(long)]
        imgfile: String,
    },
    /// Increases the stock of a product by a specified amount
    IncreaseStock {
        /// The ID of the product
        #[arg(long)]
        id: String,
        /// The amount to increase the stock by
        #[arg(long)]
        amount: i64,
    },
    /// Decreases the stock of a product by a specified amount
    DecreaseStock {
        /// The ID of the product
        #[arg(long)]
        id: String,
        /// The amount to decrease the stock by
        #[arg(long)]
        amount: i64,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();

    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .init();

    let _ =
        rustls::crypto::CryptoProvider::install_default(rustls::crypto::ring::default_provider());

    let cli = Cli::parse();

    // The PROJECT_ID environment variable is crucial for authenticating with Google Cloud
    // and specifying which Firestore project to interact with.
    // Ensure this is set in your environment or within a .env file.
    let gcp_project_id = std::env::var("PROJECT_ID").context(
        "PROJECT_ID environment variable not set. Please set it to your Google Cloud project ID.",
    )?;
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
        Commands::Delete { id } => {
            info!("Deleting product by id: {}", id);
            delete_product_by_id(&db, id.clone()).await?;
            info!("Product with id {} deleted successfully.", id);
        }
        Commands::Find { name } => {
            info!("Searching for products with name containing: {}", name);
            let products = find_products_by_name(&db, name.clone()).await?;
            if products.is_empty() {
                info!("No products found matching '{}'.", name);
            } else {
                for product in products {
                    info!("{:?}", product);
                }
            }
        }
        Commands::Add {
            name,
            price,
            quantity,
            imgfile,
        } => {
            info!("Adding new product: {}", name);
            add_product(&db, name.clone(), price, quantity, imgfile.clone()).await?;
            info!("Product '{}' added successfully.", name);
        }
        Commands::IncreaseStock { id, amount } => {
            info!("Increasing stock for product id: {} by {}", id, amount);
            increase_product_inventory(&db, id.clone(), amount).await?;
            info!("Stock increased successfully for product id: {}", id);
        }
        Commands::DecreaseStock { id, amount } => {
            info!("Decreasing stock for product id: {} by {}", id, amount);
            decrease_product_inventory(&db, id.clone(), amount).await?;
            info!("Stock decreased successfully for product id: {}", id);
        }
    }

    Ok(())
}

async fn get_products(db: &Arc<FirestoreDb>) -> anyhow::Result<Vec<Product>> {
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

async fn get_product_by_id(db: &Arc<FirestoreDb>, id: String) -> anyhow::Result<Option<Product>> {
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

async fn delete_product_by_id(db: &Arc<FirestoreDb>, id: String) -> anyhow::Result<()> {
    db.fluent()
        .delete()
        .from("inventory")
        .document_id(&id)
        .execute()
        .await
        .context(format!(
            "Failed to delete product with ID {} from Firestore",
            id
        ))?;

    Ok(())
}

async fn find_products_by_name(
    db: &Arc<FirestoreDb>,
    name: String,
) -> anyhow::Result<Vec<Product>> {
    let search_name_lower = name.to_lowercase();
    let all_products: Vec<Product> = db
        .fluent()
        .select()
        .from("inventory")
        .obj()
        .query()
        .await
        .context("Failed to query all products for name search from Firestore")?;

    let filtered_products = all_products
        .into_iter()
        .filter(|product| product.name.to_lowercase().contains(&search_name_lower))
        .collect();

    Ok(filtered_products)
}

async fn add_product(
    db: &Arc<FirestoreDb>,
    name: String,
    price: f64,
    quantity: i64,
    imgfile: String,
) -> anyhow::Result<()> {
    let product = Product {
        id: None, // Will be generated in add_or_update_firestore if not present
        name,
        price,
        quantity,
        imgfile,
        timestamp: Utc::now(),
        actualdateadded: Utc::now(),
    };

    add_or_update_firestore(db, &product).await?;

    Ok(())
}

async fn increase_product_inventory(
    db: &Arc<FirestoreDb>,
    id: String,
    amount: i64,
) -> anyhow::Result<()> {
    if let Some(mut product) = get_product_by_id(db, id.clone()).await? {
        product.quantity += amount;
        add_or_update_firestore(db, &product).await?;
        Ok(())
    } else {
        Err(anyhow::anyhow!("Product with id {} not found.", id))
    }
}

async fn decrease_product_inventory(
    db: &Arc<FirestoreDb>,
    id: String,
    amount: i64,
) -> anyhow::Result<()> {
    if let Some(mut product) = get_product_by_id(db, id.clone()).await? {
        if product.quantity >= amount {
            product.quantity -= amount;
            add_or_update_firestore(db, &product).await?;
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "Insufficient inventory for product with id {}. Current quantity: {}, Requested decrease: {}",
                id,
                product.quantity,
                amount
            ))
        }
    } else {
        Err(anyhow::anyhow!("Product with id {} not found.", id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_products() {
        let product_names = ["Test Product 1", "Test Product 2"];
        let price_range = (10.0, 20.0);
        let quantity_range = (5, 15);
        let days_ago_range = (0, 5);

        let products = generate_products(
            &product_names,
            price_range,
            quantity_range,
            days_ago_range,
            None,
        );

        assert_eq!(products.len(), 2);
        
        for product in products {
            assert!(product.price >= 10.0 && product.price < 20.0);
            assert!(product.quantity >= 5 && product.quantity < 15);
            assert!(product.name.contains("Test Product"));
        }
    }

    #[test]
    fn test_product_serialization() {
        let product = Product {
            id: Some("test-id".to_string()),
            name: "Serialization Test".to_string(),
            price: 99.99,
            quantity: 42,
            imgfile: "test.png".to_string(),
            timestamp: Utc::now(),
            actualdateadded: Utc::now(),
        };

        let serialized = serde_json::to_string(&product).expect("Failed to serialize product");
        let deserialized: Product = serde_json::from_str(&serialized).expect("Failed to deserialize product");

        assert_eq!(product.id, deserialized.id);
        assert_eq!(product.name, deserialized.name);
        assert_eq!(product.price, deserialized.price);
        assert_eq!(product.quantity, deserialized.quantity);
        assert_eq!(product.imgfile, deserialized.imgfile);
        // Note: Timestamp comparison might be tricky due to precision loss in JSON serialization,
        // so we often compare them with a small tolerance or check if they are reasonably close,
        // but for basic struct integrity, checking other fields is usually sufficient proof 
        // that serde is working for the struct.
    }
}

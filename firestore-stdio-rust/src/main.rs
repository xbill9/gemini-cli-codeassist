
use anyhow::Result;
use chrono::{DateTime, Duration, Utc};
use firestore::FirestoreDb;
use rand::Rng;
use rmcp::{
    handler::server::{tool::ToolRouter, ServerHandler},
    model::{ServerCapabilities, ServerInfo},
    schemars,
    tool,
    tool_handler,
    tool_router,
    ServiceExt,
};
use rmcp::handler::server::wrapper::Parameters;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::{error, info, warn};
use tracing_subscriber::EnvFilter;
use uuid::Uuid;

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GetMsgRequest {
    #[schemars(description = "hello world")]
    pub message: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GetProductByIdRequest {
    #[schemars(description = "The ID of the product to retrieve")]
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, schemars::JsonSchema)]
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


#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct AddProductRequest {
    #[schemars(description = "The product to add")]
    pub product: Product,
}

#[derive(Debug, Deserialize)]
struct DocName {
    name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
struct ProductList {
    products: Vec<Product>,
}


#[derive(Clone,Debug)]
struct Inventory  {
    tool_router: ToolRouter<Self>,
    db: Arc<FirestoreDb>,
}

#[tool_router]
impl Inventory {
    fn new(db: Arc<FirestoreDb>) -> Self {
        Self {
            tool_router: Self::tool_router(),
            db,
        }
    }

    #[tool(description = "Inventory API via Model Context Protocol")]
    #[tracing::instrument(skip(self))]
    async fn root(&self) -> Result<String, String> {
        info!("Handling root request");
        Ok("🍎 Hello! This is the Cymbal Superstore Inventory API.".to_string())
    }

    #[tool(description = "Inventory API Health Status of Inventory API")]
    #[tracing::instrument(skip(self))]
    async fn health(&self) -> Result<String, String> {
        info!("Handling health check");
        Ok("✅ ok".to_string())
    }

    #[tool(description = "Inventory API via Model Context Protocol")]
    #[tracing::instrument(skip(self))]
    async fn echo(
        &self,
        Parameters(GetMsgRequest { message }): Parameters<GetMsgRequest>,
    ) -> String {
        info!("Handling echo request with message: {}", message);
        let msg = format!("Inventory MCP! {}",message);
        msg
    }

    #[tool(description = "Seeds the database with initial product data.")]
    #[tracing::instrument(skip(self))]
    async fn seed(&self) -> Result<String, String> {
        info!("Starting database seed");
        let old_products_to_add: Vec<Product> = generate_products(
            &[
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
            ],
            1,
            501,
            90,
            365,
        );

        for product in old_products_to_add {
            if let Err(e) = add_or_update_firestore(&self.db, &product).await {
                error!("Error adding/updating product: {:?}", e);
                return Err(format!("Failed to add/update product: {:?}", e));
            }
        }

        let recent_products_to_add: Vec<Product> = generate_products(
            &[
                "Parmesan Crisps",
                "Pineapple Kombucha",
                "Maple Almond Butter",
                "Mint Chocolate Cookies",
                "White Chocolate Caramel Corn",
                "Acai Smoothie Packs",
                "Smores Cereal",
                "Peanut Butter and Jelly Cups",
            ],
            1,
            101,
            0,
            6,
        );

        for product in recent_products_to_add {
            if let Err(e) = add_or_update_firestore(&self.db, &product).await {
                error!("Error adding/updating product: {:?}", e);
                return Err(format!("Failed to add/update product: {:?}", e));
            }
        }

        let oos_products_to_add: Vec<Product> = generate_products(
            &["Wasabi Party Mix", "Jalapeno Seasoning"],
            0,
            1,
            0,
            6,
        );

        for product in oos_products_to_add {
            if let Err(e) = add_or_update_firestore(&self.db, &product).await {
                error!("Error adding/updating product: {:?}", e);
                return Err(format!("Failed to add/update product: {:?}", e));
            }
        }
        
        info!("Database seed completed successfully");
        Ok("Database seeded successfully.".to_string())
    }

    #[tool(description = "Retrieves a list of all products.")]
    #[tracing::instrument(skip(self))]
    async fn get_products(&self) -> Result<String, String> {
        info!("Retrieving all products");
        let products: Vec<Product> = self.db
            .fluent()
            .select()
            .from("inventory")
            .obj()
            .query()
            .await
            .map_err(|e| format!("Failed to retrieve products: {:?}", e))?;

        let product_list = ProductList { products };
        serde_json::to_string(&product_list)
            .map_err(|e| format!("Failed to serialize product list: {:?}", e))
    }

    #[tool(description = "Retrieves a product by its ID.")]
    #[tracing::instrument(skip(self))]
    async fn get_product_by_id(
        &self,
        Parameters(GetProductByIdRequest { id }): Parameters<GetProductByIdRequest>,
    ) -> Result<String, String> {
        info!("Retrieving product by ID: {}", id);
        let product: Option<Product> = self.db
            .fluent()
            .select()
            .by_id_in("inventory")
            .obj()
            .one(&id)
            .await
            .map_err(|e| format!("Failed to retrieve product: {:?}", e))?;

        if let Some(product) = product {
            serde_json::to_string(&product)
                .map_err(|e| format!("Failed to serialize product: {:?}", e))
        } else {
            warn!("Product with ID {} not found", id);
            Err(format!("Product with ID {} not found", id))
        }
    }

    #[tool(description = "Adds a product to the database.")]
    #[tracing::instrument(skip(self))]
    async fn add_product(
        &self,
        Parameters(AddProductRequest { product }): Parameters<AddProductRequest>,
    ) -> Result<String, String> {
        info!("Adding new product: {}", product.name);
        let mut product_to_insert = product.clone();
        product_to_insert.id = None;
        let product_id = Uuid::new_v4().to_string();
        self.db
            .fluent()
            .insert()
            .into("inventory")
            .document_id(&product_id)
            .object(&product_to_insert)
            .execute::<()>()
            .await
            .map_err(|e| format!("Failed to add product: {:?}", e))?;
        info!("Product added successfully with ID: {}", product_id);
        Ok(format!("Product added with ID: {}", product_id))
    }
}

#[tool_handler]
impl ServerHandler for Inventory {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some("Inventory Management API via stdio transport on MCP".into()),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}


// helper functions

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
                    let mut product_to_update = product.clone();
                    product_to_update.id = None;
                    db.fluent()
                        .update()
                        .in_col("inventory")
                        .document_id(doc_id)
                        .object(&product_to_update)
                        .execute::<()>()
                        .await?;        }
    } else {
        let mut product_to_insert = product.clone();
        product_to_insert.id = None;
        let product_id = Uuid::new_v4().to_string();
        db.fluent()
            .insert()
            .into("inventory")
            .document_id(&product_id)
            .object(&product_to_insert)
            .execute::<()>()
            .await?;
    }
    Ok(())
}


fn generate_products(
    product_names: &[&str],
    min_quantity: i64,
    max_quantity: i64,
    min_days_ago: i64,
    max_days_ago: i64,
) -> Vec<Product> {
    let mut rng = rand::thread_rng();
    product_names
        .iter()
        .map(|&product_name| Product {
            id: None,
            name: product_name.to_string(),
            price: rng.gen_range(1.0..11.0),
            quantity: rng.gen_range(min_quantity..max_quantity),
            imgfile: format!(
                "product-images/{}.png",
                product_name.replace(' ', "").to_lowercase()
            ),
            timestamp: Utc::now() - Duration::days(rng.gen_range(min_days_ago..max_days_ago)),
            actualdateadded: Utc::now(),
        })
        .collect()
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .json()
        .init();

    info!("🍏 Cymbal Superstore: Inventory API Starting");
    let gcp_project_id = std::env::var("PROJECT_ID").expect("PROJECT_ID must be set");
    let db = Arc::new(FirestoreDb::new(&gcp_project_id).await?);

    let service = Inventory::new(db.clone());
    let transport = rmcp::transport::stdio();
    let server = service.serve(transport).await?;
    server.waiting().await?;

    info!("🍏 Cymbal Superstore: Inventory API completed");
    Ok(())
}




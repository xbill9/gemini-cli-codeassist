use anyhow::{Context, Result};
use axum::Router;
use chrono::{DateTime, Duration, Utc};
use firestore::FirestoreDb;
use rand::Rng;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::{
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router,
    transport::streamable_http_server::{
        StreamableHttpService, session::local::LocalSessionManager,
    },
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::{error, info};
use uuid::Uuid;

const INVENTORY_COLLECTION: &str = "inventory";

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
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

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
struct ProductList {
    products: Vec<Product>,
}

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

#[derive(Clone, Debug)]
struct Inventory {
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
    async fn root(&self) -> Result<String, String> {
        Ok("🍎 Hello! This is the Cymbal Superstore Inventory API.".to_string())
    }

    #[tool(description = "Health Status of Inventory API")]
    async fn health(&self) -> Result<String, String> {
        Ok("✅ ok".to_string())
    }

    #[tool(description = "Inventory API via Model Context Protocol")]
    async fn echo(
        &self,
        Parameters(GetMsgRequest { message }): Parameters<GetMsgRequest>,
    ) -> String {
        let msg = format!("Inventory MCP! {}", message);
        msg
    }

    #[tool(description = "Seeds the database with initial product data.")]
    async fn seed(&self) -> Result<String, String> {
        let product_batches = vec![
            (
                vec![
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
            ),
            (
                vec![
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
            ),
            (vec!["Wasabi Party Mix", "Jalapeno Seasoning"], 0, 1, 0, 6),
        ];

        for (names, min_q, max_q, min_d, max_d) in product_batches {
            let products = generate_products(&names, min_q, max_q, min_d, max_d);
            for product in products {
                if let Err(e) = add_or_update_firestore(&self.db, &product).await {
                    error!("Error adding/updating product: {}", e);
                    return Err(format!("Failed to add/update product: {}", e));
                }
            }
        }

        Ok("Database seeded successfully.".to_string())
    }

    #[tool(description = "Retrieves a list of all products.")]
    async fn get_products(&self) -> Result<String, String> {
        let products: Vec<Product> = self
            .db
            .fluent()
            .select()
            .from(INVENTORY_COLLECTION)
            .obj()
            .query()
            .await
            .map_err(|e| {
                error!("Failed to retrieve products: {}", e);
                e.to_string()
            })?;

        let product_list = ProductList { products };
        serde_json::to_string(&product_list).map_err(|e| {
            error!("Failed to serialize product list: {}", e);
            e.to_string()
        })
    }

    #[tool(description = "Retrieves a product by its ID.")]
    async fn get_product_by_id(
        &self,
        Parameters(GetProductByIdRequest { id }): Parameters<GetProductByIdRequest>,
    ) -> Result<String, String> {
        let product: Option<Product> = self
            .db
            .fluent()
            .select()
            .by_id_in(INVENTORY_COLLECTION)
            .obj()
            .one(&id)
            .await
            .map_err(|e| {
                error!("Failed to retrieve product: {}", e);
                e.to_string()
            })?;

        if let Some(product) = product {
            serde_json::to_string(&product).map_err(|e| {
                error!("Failed to serialize product: {}", e);
                e.to_string()
            })
        } else {
            let msg = format!("Product with ID {} not found", id);
            error!("{}", msg);
            Err(msg)
        }
    }
}

#[tool_handler]
impl ServerHandler for Inventory {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some("Inventory Management API via MCP".into()),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

// helper functions
async fn add_or_update_firestore(db: &Arc<FirestoreDb>, product: &Product) -> Result<()> {
    let existing_docs: Vec<DocName> = db
        .fluent()
        .select()
        .from(INVENTORY_COLLECTION)
        .filter(|q| q.field("name").eq(&product.name))
        .limit(1)
        .obj()
        .query()
        .await
        .context("Failed to query for existing product by name")?;

    if let Some(doc) = existing_docs.first() {
        if let Some(doc_id) = doc.name.split('/').next_back()
            && !doc_id.is_empty()
        {
            info!("Updating product: {}", product.name);
            db.fluent()
                .update()
                .in_col(INVENTORY_COLLECTION)
                .document_id(doc_id)
                .object(product)
                .execute::<()>()
                .await
                .context("Failed to update product in Firestore")?;
        }
    } else {
        let product_id = Uuid::new_v4().to_string();
        info!("Adding new product: {}", product.name);
        db.fluent()
            .insert()
            .into(INVENTORY_COLLECTION)
            .document_id(&product_id)
            .object(product)
            .execute::<()>()
            .await
            .context("Failed to insert new product into Firestore")?;
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
        .with_target(false)
        .compact()
        .init();

    let _ =
        rustls::crypto::CryptoProvider::install_default(rustls::crypto::ring::default_provider());
    dotenv::dotenv().ok();

    info!("Starting server...");

    let gcp_project_id = std::env::var("PROJECT_ID").expect("PROJECT_ID must be set");
    let db = Arc::new(FirestoreDb::new(&gcp_project_id).await?);
    let db_for_service = db.clone(); // Clone db for the service closure

    let service = StreamableHttpService::new(
        move || Ok(Inventory::new(db_for_service.clone())),
        LocalSessionManager::default().into(),
        Default::default(),
    );

    let app = Router::new()
        .nest_service("/mcp", service)
        .with_state(db.clone());

    let port = std::env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{}", port)).await?;
    info!(
        "🍏 Cymbal Superstore: Inventory API running on port: {}",
        port
    );
    axum::serve(listener, app).await?;

    Ok(())
}

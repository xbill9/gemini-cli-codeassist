use google_cloud_gax::paginator::Paginator as _;
use google_cloud_secretmanager_v1::client::SecretManagerService;

use rmcp::{
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    tool, tool_handler, tool_router,
    transport::streamable_http_server::{
        StreamableHttpService, session::local::LocalSessionManager,
    },
};

use anyhow::Result;
use std::env;
use std::net::SocketAddr;
use tracing::{error, info};

#[derive(Clone)]
pub struct GCPClient {
    client: SecretManagerService,
    tool_router: ToolRouter<Self>,
}

#[tool_router]
impl GCPClient {
    pub fn new(client: SecretManagerService) -> Self {
        Self {
            client,
            tool_router: Self::tool_router(),
        }
    }

    #[tool(description = "GCP SDK client call with Model Context Protocol")]
    pub async fn list_locations(&self) -> String {
        let project_id = match std::env::var("PROJECT_ID") {
            Ok(id) => id,
            Err(e) => {
                error!("PROJECT_ID not set: {}", e);
                return format!("Error: PROJECT_ID environment variable not set: {}", e);
            }
        };

        info!("Starting client API call Project {}", project_id);

        let mut items = self
            .client
            .list_locations()
            .set_name(format!("projects/{}", project_id))
            .by_page();

        let mut output = String::new();
        while let Some(page) = items.next().await {
            match page {
                Ok(p) => {
                    for location in p.locations {
                        output.push_str(&format!("{}\n", location.name));
                    }
                }
                Err(e) => {
                    error!("Error listing locations: {}", e);
                    return format!("Error listing locations: {}", e);
                }
            }
        }

        info!("Completed client API call Project {}", project_id);
        if output.is_empty() {
            format!("No locations found for project {}.", project_id)
        } else {
            format!(
                "Successfully listed locations for project {}:\n{}",
                project_id, output
            )
        }
    }
}

#[tool_handler]
impl ServerHandler for GCPClient {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "A minimum viable Model Context Protocol Google Cloud SDK call over https transport in Rust ".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing with JSON format
    tracing_subscriber::fmt().json().init();

    // Initialize GCP Client once
    let client = SecretManagerService::builder().build().await?;

    // Create the service with a factory that reuses the client
    let service = StreamableHttpService::new(
        move || Ok(GCPClient::new(client.clone())),
        LocalSessionManager::default().into(),
        Default::default(),
    );

    let router = axum::Router::new().nest_service("/mcp", service);

    // Get port from environment variable, default to 8080
    let port = env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let addr: SocketAddr = format!("0.0.0.0:{}", port).parse()?;

    info!("listening on {}", addr);

    let tcp_listener = tokio::net::TcpListener::bind(addr).await?;

    if let Err(e) = axum::serve(tcp_listener, router).await {
        error!("server error: {}", e);
    }

    Ok(())
}

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
use tracing::{info, error};
use tracing_subscriber::EnvFilter;

#[derive(Clone)]
pub struct GCPClient {
    tool_router: ToolRouter<Self>,
    project_id: String,
    client: SecretManagerService,
}

#[tool_router]
impl GCPClient {
    pub fn new(project_id: String, client: SecretManagerService) -> Self {
        Self {
            tool_router: Self::tool_router(),
            project_id,
            client,
        }
    }

    #[tool(description = "GCP SDK client call with Model Context Protocol")]
    pub async fn list_locations(&self) -> String {
        let project_id = &self.project_id;
        
        info!(%project_id, "Starting client API call");

        let mut items = self.client
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
                Err(e) => return format!("Error listing locations: {}", e),
            }
        }

        info!(%project_id, "Completed client API call");
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
    tracing_subscriber::fmt()
        .json()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    // Fail fast if PROJECT_ID is not set
    let project_id = env::var("PROJECT_ID")
        .map_err(|_| anyhow::anyhow!("PROJECT_ID environment variable must be set"))?;

    // Initialize the client once
    let client = SecretManagerService::builder().build().await?;

    let service = StreamableHttpService::new(
        move || Ok(GCPClient::new(project_id.clone(), client.clone())),
        LocalSessionManager::default().into(),
        Default::default(),
    );

    let router = axum::Router::new().nest_service("/mcp", service);

    // Get port from environment variable, default to 8080
    let port = env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let addr: SocketAddr = format!("0.0.0.0:{}", port).parse()?;

    info!(%addr, "listening");

    let tcp_listener = tokio::net::TcpListener::bind(addr).await?;

    if let Err(e) = axum::serve(tcp_listener, router).await {
        error!(error = %e, "server error");
    }

    Ok(())
}
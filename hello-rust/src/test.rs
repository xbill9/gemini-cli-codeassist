use anyhow::Result;
use rmcp::{
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    transport::streamable_http_server::{session::local::LocalSessionManager, StreamableHttpService},
    tool,
    tool_router,
    tool_handler,
    schemars,
};
use rmcp::handler::server::wrapper::Parameters;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

const BIND_ADDRESS: &str = "0.0.0.0:8080";

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GetAlertsRequest {
    #[schemars(description = "hello world")]
    pub state: String,
}

#[derive(Debug, Clone)]
struct HelloWorld {
    tool_router: ToolRouter<Self>,
}

#[tool_router]
impl HelloWorld {
    fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }

    #[tool(description = "Hello World via Model Context Protocol")]
    async fn greeting(
        &self,
        Parameters(GetAlertsRequest { state }): Parameters<GetAlertsRequest>,
    ) -> String {
        let msg = format!("Hello World MCP!");
        msg
    }
}

#[tool_handler]
impl ServerHandler for HelloWorld {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some("A simple Hello World MCP".into()),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

async fn run() -> anyhow::Result<()> {
    // Initialize tracing subscriber for logging
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info,min_rust=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer().pretty())
        .init();

    tracing::info!("MCP Starting server on {}", BIND_ADDRESS);

    let service = StreamableHttpService::new(
        || Ok(HelloWorld::new()),
        LocalSessionManager::default().into(),
        Default::default(),
    );

    let router = axum::Router::new().nest_service("/mcp", service);
    let tcp_listener = tokio::net::TcpListener::bind(BIND_ADDRESS).await?;

    tracing::info!("MCP Server listening on {}", BIND_ADDRESS);

    let _ = axum::serve(tcp_listener, router)
        .with_graceful_shutdown(async {
            if let Err(e) = tokio::signal::ctrl_c().await {
                tracing::error!("Failed to listen for ctrl-c: {}", e)
            }
            tracing::info!("Shutting down...");
        })
        .await;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    run().await
}


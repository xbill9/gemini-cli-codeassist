use anyhow::Result;
use rmcp::{
    ServiceExt,
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    tool, tool_handler, tool_router,
};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

/// HelloWorld is a simple implementation of the Model Context Protocol (MCP) server.
#[derive(Clone, Debug)]
pub struct HelloWorld {
    tool_router: ToolRouter<Self>,
}

#[tool_router]
impl HelloWorld {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }

    #[tool(description = "Hello World via Model Context Protocol")]
    async fn hello_mcp(&self) -> String {
        "Hello World MCP!".to_string()
    }

    #[tool(description = "Hello Rust via Model Context Protocol")]
    async fn rust_mcp(&self) -> String {
        "Hello World Rust MCP!".to_string()
    }
    #[tool(description = "Just prints Z via Model Context Protocol")]
    async fn z(&self) -> String {
        "Z".to_string()
    }

    #[tool(description = "prints version via Model Context Protocol")]
    async fn version(&self) -> String {
        const VERSION: &str = env!("CARGO_PKG_VERSION");
        format!("MyProgram v{}", VERSION)
    }
}

impl Default for HelloWorld {
    fn default() -> Self {
        Self::new()
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

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing subscriber for logging
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info,min_rust=debug".into()),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(std::io::stderr)
                .pretty(),
        )
        .init();

    tracing::info!("MCP Starting server on stdio");

    let service = HelloWorld::new();
    let transport = rmcp::transport::stdio();
    let server = service.serve(transport).await?;
    server.waiting().await?;

    Ok(())
}

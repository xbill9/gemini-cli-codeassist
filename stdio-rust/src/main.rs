use anyhow::Result;
use rmcp::{
    handler::server::{tool::ToolRouter, wrapper::Parameters, ServerHandler},
    model::{ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router, ServiceExt,
};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GetMsgRequest {
    #[schemars(description = "hello world")]
    pub message: String,
}

#[derive(Clone)]
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
        Parameters(GetMsgRequest { message }): Parameters<GetMsgRequest>,
    ) -> String {
        let msg = format!("Hello World MCP! {}", message);
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

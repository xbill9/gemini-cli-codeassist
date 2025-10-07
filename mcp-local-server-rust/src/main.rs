use anyhow::Result;
use rmcp::{
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    tool, tool_handler, tool_router,
    transport::streamable_http_server::{
        StreamableHttpService, session::local::LocalSessionManager,
    },
};
use std::env;
use std::net::SocketAddr;

impl Default for MinRust {
    fn default() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }
}

#[derive(Clone)]
pub struct MinRust {
    tool_router: ToolRouter<Self>,
}

#[tool_router]
impl MinRust {
    #[tool(description = "Hello World via Model Context Protocol")]
    pub async fn hellomcp(&self) -> String {
        "Hello World MCP!".to_string()
    }

    #[tool(description = "Hello Rust via Model Context Protocol")]
    pub async fn hellorust(&self) -> String {
        "Hello World Rust MCP!".to_string()
    }

    #[tool(description = "Just prints Z via Model Context Protocol")]
    pub async fn z(&self) -> String {
        "Z".to_string()
    }

    #[tool(description = "Print version environment variable via Model Context Protocol")]
    pub async fn version(&self) -> String {
        format!("MyProgram v{}", env!("CARGO_PKG_VERSION"))
    }
}

#[tool_handler]
impl ServerHandler for MinRust {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "A minimum viable Model Context Protocol MCP with https transport in Rust".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let service = StreamableHttpService::new(
        || Ok(MinRust::default()),
        LocalSessionManager::default().into(),
        Default::default(),
    );

    let router = axum::Router::new().nest_service("/mcp", service);

    // Get port from environment variable, default to 8080
    let port = env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let addr: SocketAddr = format!("0.0.0.0:{}", port).parse()?;

    println!("listening on {}", addr);

    let tcp_listener = tokio::net::TcpListener::bind(addr).await?;

    if let Err(e) = axum::serve(tcp_listener, router).await {
        eprintln!("server error: {}", e);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_hellomcp() {
        let min_rust = MinRust::default();
        assert_eq!(min_rust.hellomcp().await, "Hello World MCP!");
    }

    #[tokio::test]
    async fn test_hellorust() {
        let min_rust = MinRust::default();
        assert_eq!(min_rust.hellorust().await, "Hello World Rust MCP!");
    }

    #[tokio::test]
    async fn test_z() {
        let min_rust = MinRust::default();
        assert_eq!(min_rust.z().await, "Z");
    }

    #[tokio::test]
    async fn test_version() {
        let min_rust = MinRust::default();
        // We can't assert the exact version as it comes from Cargo.toml, but we can check the format.
        assert!(min_rust.version().await.starts_with("MyProgram v"));
    }
}

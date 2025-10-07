use anyhow::Result;
use rmcp::{
    ServiceExt,
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    tool, tool_handler, tool_router,
};

impl Default for MinRust {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct MinRust {
    tool_router: ToolRouter<Self>,
}

#[tool_router]
impl MinRust {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }

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
        const VERSION: &str = env!("CARGO_PKG_VERSION");
        format!("MyProgram v{}", VERSION)
    }
}

#[tool_handler]
impl ServerHandler for MinRust {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "A minimum viable Model Context Protocol MCP with stdio transport in Rust".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let service = MinRust::default();
    let transport = rmcp::transport::stdio();
    let server = service.serve(transport).await?;
    server.waiting().await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::MinRust;

    #[test]
    fn test_min_rust_new() {
        let min_rust = MinRust::new();
        assert!(!min_rust.tool_router.map.is_empty());
    }

    #[test]
    fn test_min_rust_default() {
        let min_rust = MinRust::default();
        assert!(!min_rust.tool_router.map.is_empty());
    }

    #[tokio::test]
    async fn test_hellomcp() {
        let min_rust = MinRust::new();
        assert_eq!(min_rust.hellomcp().await, "Hello World MCP!".to_string());
    }

    #[tokio::test]
    async fn test_hellorust() {
        let min_rust = MinRust::new();
        assert_eq!(
            min_rust.hellorust().await,
            "Hello World Rust MCP!".to_string()
        );
    }

    #[tokio::test]
    async fn test_z() {
        let min_rust = MinRust::new();
        assert_eq!(min_rust.z().await, "Z".to_string());
    }

    #[tokio::test]
    async fn test_version() {
        let min_rust = MinRust::new();
        let expected_version = format!("MyProgram v{}", env!("CARGO_PKG_VERSION"));
        assert_eq!(min_rust.version().await, expected_version);
    }
}

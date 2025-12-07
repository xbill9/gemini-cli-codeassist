use anyhow::Result;
use rmcp::{
    ServiceExt,
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{CallToolRequestParam, CallToolResult, ServerCapabilities, ServerInfo},
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

fn init_tracing() {
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
}

fn extract_text_from_result(result: CallToolResult) -> String {
    result
        .content
        .first()
        .and_then(|annotated| match &annotated.raw {
            rmcp::model::RawContent::Text(raw_text_content) => Some(raw_text_content.text.clone()),
            _ => None,
        })
        .unwrap_or_else(|| "(No text content found)".to_string())
}

// Client-side implementation
async fn run_client() -> Result<(), anyhow::Error> {
    init_tracing();

    tracing::info!("MCP Client starting and spawning server...");

    // Get the path to the current executable
    let current_exe = std::env::current_exe()?;

    let mut cmd = tokio::process::Command::new(current_exe);
    cmd.arg("server") // Argument to indicate running as server
        .kill_on_drop(true)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit());

    // Create a new client that communicates with an MCP server
    // launched as a child process.
    let client = ().serve(rmcp::transport::TokioChildProcess::new(cmd)?).await?;

    // Get and log server roots (tools)
    tracing::info!("Querying server tools...");
    let tool_list_result = client.list_tools(None).await?;
    tracing::info!("Server Tools: {:#?}", tool_list_result.tools);

    // Call 'hello_mcp' on the server
    tracing::info!("Calling 'hello_mcp' on the server...");
    let request_param_hello = CallToolRequestParam {
        name: "hello_mcp".to_string().into(),
        arguments: Some(serde_json::Map::new()),
    };
    let response_result_hello: CallToolResult = client.call_tool(request_param_hello).await?;
    let response_hello: String = extract_text_from_result(response_result_hello);
    tracing::info!("Server response (hello_mcp): {}", response_hello);

    // Call 'version' tool
    tracing::info!("Calling 'version' on the server...");
    let request_param_version = CallToolRequestParam {
        name: "version".to_string().into(),
        arguments: Some(serde_json::Map::new()),
    };
    let response_result_version: CallToolResult = client.call_tool(request_param_version).await?;
    let response_version: String = extract_text_from_result(response_result_version);
    tracing::info!("Server response (version): {}", response_version);

    tracing::info!("MCP Client finished.");

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    // Check for a command-line argument to decide whether to run as server or client
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 && args[1] == "server" {
        // Server mode
        init_tracing();

        tracing::info!("MCP Starting server on stdio");

        let service = HelloWorld::new();
        let transport = rmcp::transport::stdio();
        let server = service.serve(transport).await?;
        server.waiting().await?;
    } else {
        // Client mode (default)
        run_client().await?;
    }

    Ok(())
}

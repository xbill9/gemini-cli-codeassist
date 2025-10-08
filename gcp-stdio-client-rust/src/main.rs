use google_cloud_gax::paginator::Paginator as _;
use google_cloud_secretmanager_v1::client::SecretManagerService;

use rmcp::{
    ServiceExt,
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    tool_router,
};

use anyhow::Result;

use rmcp_macros::{tool, tool_handler};

impl Default for GCPClient {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct GCPClient {
    tool_router: ToolRouter<Self>,
}

#[tool_router]
impl GCPClient {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }

    #[tool(description = "GCP SDK client call with Model Context Protocol")]
    pub async fn list_locations(&self) -> String {
        let project_id = match std::env::var("PROJECT_ID") {
            Ok(id) => id,
            Err(e) => return format!("Error: PROJECT_ID environment variable not set: {}", e),
        };

        let client = match SecretManagerService::builder().build().await {
            Ok(c) => c,
            Err(e) => return format!("Error building SecretManagerService client: {}", e),
        };

        println!("Starting client API call Project {}", project_id);

        let mut items = client
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

        println!("Completed client API call Project {}", project_id);
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
                "A minimum viable Model Context Protocol Google Cloud SDK call over stdio transport in Rust ".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let service = GCPClient::default();
    let transport = rmcp::transport::stdio();
    let server = service.serve(transport).await?;
    server.waiting().await?;

    Ok(())
}

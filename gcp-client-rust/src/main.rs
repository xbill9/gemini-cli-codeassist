use anyhow::Context;
use google_cloud_gax::paginator::Paginator as _;
use google_cloud_secretmanager_v1::client::SecretManagerService;
use tracing::info;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize structured logging (JSON format for Cloud Logging)
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .json()
        .init();

    // validate PROJECT_ID
    let project_id =
        std::env::var("PROJECT_ID").context("PROJECT_ID environment variable must be set")?;

    // Google Cloud Client API to secret manager
    let client = SecretManagerService::builder().build().await?;

    info!(project_id, "Starting client API call");
    // make client call
    let mut items = client
        .list_locations()
        .set_name(format!("projects/{project_id}"))
        .by_page();
    while let Some(page) = items.next().await {
        let page = page?;
        for location in page.locations {
            info!(location_name = location.name, "Found location");
        }
    }

    info!(project_id, "Completed client API call");

    // return OK
    Ok(())
}

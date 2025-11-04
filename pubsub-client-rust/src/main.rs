use google_cloud_pubsub::client::{Client, ClientConfig};
use log::{info, warn};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    pretty_env_logger::init();
    info!("Starting pubsub client");

    let config = ClientConfig::default().with_auth().await?;
    let client = Client::new(config).await?;
    let project_id = std::env::var("PROJECT_ID")?;

    info!("Fetching topics for project `{}`", project_id);

    let topics = client.get_topics(None).await?;

    if topics.is_empty() {
        warn!("No topics found in project `{}`", project_id);
    } else {
        info!("Found {} topics in project `{}`:", topics.len(), project_id);
        for topic in topics {
            info!("  - {}", topic);
        }
    }

    info!("Pubsub client finished successfully");
    Ok(())
}

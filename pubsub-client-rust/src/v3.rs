use google_cloud_pubsub::client::{Client, ClientConfig};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = ClientConfig::default().with_auth().await.unwrap();
    let client = Client::new(config).await.unwrap();
    let project_id = std::env::var("PROJECT_ID")?;

    println!("Topics in project {}:", project_id);

    let topics = client.get_topics(None).await?;

    if topics.is_empty() {
        println!("No topics found.");
    } else {
        for topic in topics {
            println!("  {}", topic);
        }
    }

    Ok(())
}

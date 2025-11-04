mod v1;

use google_cloud_pubsub::client::Client;
use google_cloud_pubsub::topic::Topic;
use google_cloud_secretmanager_v1::client::SecretManagerService;
use log::{debug, info, warn};
use google_cloud_pubsub::publisher::Message;
use crate::v1::{create_test_topic, cleanup_test_topic};


#[tokio::main]
async fn main() -> anyhow::Result<()> {
    pretty_env_logger::init();
    info!("(main) Starting up!");

    let (client, topic) = create_test_topic().await?;
    info!("(main) Created topic: {}", topic.name);

    let publisher = topic.new_publisher(None);
    let message_id = publisher.publish(Message::new(b"hello world")).await?.message_id;
    info!("(main) Published message with id: {}", message_id);

    publisher.shutdown().await;

    cleanup_test_topic(&client, topic.name.to_string()).await?;
    info!("(main) Cleaned up topic: {}", topic.name);

    Ok(())
}

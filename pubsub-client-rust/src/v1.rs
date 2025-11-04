use google_cloud_pubsub::client::Client;
use google_cloud_pubsub::topic::Topic;
use google_cloud_secretmanager_v1::client::SecretManagerService;
use rand::{Rng, distributions::Alphanumeric};

pub async fn run_topic_examples(topic_names: &mut Vec<String>) -> anyhow::Result<()> {
    let config = google_cloud_pubsub::client::ClientConfig::default();
    let client = Client::new(config).await?;
    let project_id = std::env::var("PROJECT_ID")?;

    let id = random_topic_id();
    topic_names.push(format!("projects/{project_id}/topics/{id}"));
    create_topic_sample(&client, &project_id, &id).await?;

    Ok(())
}

pub async fn cleanup_test_topic(client: &Client, topic_name: String) -> anyhow::Result<()> {
    let topic = client.topic(&topic_name);
    topic.delete(None).await?;
    Ok(())
}

pub async fn create_test_topic() -> anyhow::Result<(Client, Topic)> {
    let project_id = std::env::var("PROJECT_ID")?;
    let config = google_cloud_pubsub::client::ClientConfig::default();
    let client = Client::new(config).await?;


    let topic_id = random_topic_id();
    let now = chrono::Utc::now().timestamp().to_string();

    let topic = client.topic(&format!("projects/{project_id}/topics/{topic_id}"));
    let mut labels = std::collections::HashMap::new();
    labels.insert("integration-test".to_string(), "true".to_string());
    labels.insert("create-time".to_string(), now);
    let topic_config = google_cloud_pubsub::topic::TopicConfig { labels: labels, ..Default::default() };
    topic.create(Some(topic_config), None).await?;
    println!("success on create_topic(): {topic:?}");

    Ok((client, topic))
}

pub async fn cleanup_stale_topics(client: &Client, project_id: &str) -> anyhow::Result<()> {
    let stale_deadline = chrono::Utc::now() - chrono::Duration::hours(48);

    let topic_names = client
        .get_topics(None)
        .await?;

    let mut pending = Vec::new();
    let mut names = Vec::new();
    for topic_name in topic_names {
        // Fetch topic details to access labels
        let Some(fetched_topic_config) = client.get_topic(&topic_name, None).await? else {
            continue;
        };

        if fetched_topic_config
            .labels
            .get("integration-test")
            .is_some_and(|v| v == "true")
            && fetched_topic_config
                .labels
                .get("create-time")
                .and_then(|v| v.parse::<i64>().ok())
                .and_then(|s| chrono::DateTime::from_timestamp(s, 0))
                .is_some_and(|create_time| create_time < stale_deadline)
        {
            let client = client.clone();
            let name = fetched_topic_config.name.clone();
            pending.push(tokio::spawn(async move {
                cleanup_test_topic(&client, name).await
            }));
            names.push(fetched_topic_config.name);
        }
    }

    Ok(())
}

pub const TOPIC_ID_LENGTH: usize = 255;

fn random_topic_id() -> String {
    let prefix = "topic-";
    let topic_id: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(TOPIC_ID_LENGTH - prefix.len())
        .map(char::from)
        .collect();
    format!("{prefix}{topic_id}")
}

pub async fn create_topic_sample(client: &Client, project_id: &str, topic_id: &str) -> anyhow::Result<()> {
    let topic = client.topic(&format!("projects/{project_id}/topics/{topic_id}"));
    topic.create(None, None).await?;

    println!("successfully created topic {topic:?}");
    Ok(())
}

    async fn topic_examples() -> anyhow::Result<()> {
        let config = google_cloud_pubsub::client::ClientConfig::default();
        let client = Client::new(config).await?;

        let mut topics = Vec::new();
        let result = run_topic_examples(&mut topics).await;
        // Ignore cleanup errors.
        for name in topics.into_iter() {
            if let Err(e) = cleanup_test_topic(&client, name.clone()).await {
                println!("Error cleaning up test topic {name}: {e:?}");
            }
        }
        result
    }


/*
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _project_id = std::env::var("PROJECT_ID")?;
    let _secret_manager_client = SecretManagerService::builder().build().await?;
    let config = google_cloud_pubsub::client::ClientConfig::default();
    let client = Client::new(config).await?;

    let mut topics_to_clean_up = Vec::new();
    run_topic_examples(&mut topics_to_clean_up).await?;

//    println!("Topics in project {}:", project_id);

//    let topics = client.get_topics(None).await?;

//    if topics.is_empty() {
//        println!("No topics found.");
//    } else {
//        for topic in topics {
//            println!("  {}", topic);
//        }
//    }

    // Clean up topics created by run_topic_examples
    for name in topics_to_clean_up.into_iter() {
        if let Err(e) = cleanup_test_topic(&client, name.clone()).await {
            println!("Error cleaning up test topic {}: {:?}", name, e);
        }
    }

    Ok(())
}
*/

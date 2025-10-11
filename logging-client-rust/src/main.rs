use google_cloud_api::model::MonitoredResource;
use google_cloud_logging_type::model::LogSeverity;
use google_cloud_logging_v2::client::LoggingServiceV2;
use google_cloud_logging_v2::model::LogEntry;
use google_cloud_secretmanager_v1::client::SecretManagerService;
use log::{debug, info, warn};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();
    info!("(main) Starting up!");
    // Google Cloud Client API to secret manager
    let _sclient = SecretManagerService::builder().build().await?;

    // Initialize the logging client.
    let client = LoggingServiceV2::builder().build().await?;

    let project_id =
        std::env::var("PROJECT_ID").expect("PROJECT_ID environment variable must be set"); // IMPORTANT: Set your actual GCP Project ID as an environment variable
    debug!("(main) Project ID: {}", project_id);
    let log_name = std::env::var("LOG_NAME").unwrap_or_else(|_| "my-rust-app".to_string()); // A specific log name for your application, defaults to 'my-rust-app'

    debug!("(main) Log Name: {}", log_name);
    let mut labels = HashMap::new();
    labels.insert("environment".to_string(), "development".to_string());
    labels.insert("version".to_string(), "1.0.0".to_string());

    // Define the log entry with structured (JSON) payload and labels
    let log_entry = LogEntry::new()
        .set_log_name(format!("projects/{}/logs/{}", project_id, log_name))
        .set_severity(LogSeverity::Warning)
        .set_text_payload("This is a log entry from my Rust application.".to_string())
        // .set_json_payload(
        //     serde_json::from_value::<google_cloud_wkt::Struct>(serde_json::json!({
        //         "message": "This is a structured log entry.",
        //         "event_id": "12345",
        //         "status": "success"
        //     }))
        //     .unwrap(),
        // )
        .set_resource(
            MonitoredResource::new()
                .set_type("global".to_string())
                .set_labels(HashMap::<String, String>::new()),
        )
        .set_labels(labels);

    warn!("(main) This is a warning message.");
    // Write the log entry
    // The second and third arguments (partial_success, dry_run) are optional
    let response = client
        .write_log_entries()
        .set_entries(vec![log_entry])
        .set_partial_success(false)
        .set_dry_run(false)
        .send()
        .await;

    if let Err(e) = response {
        log::error!("(main) Failed to write log entries: {}", e);
        return Err(e.into());
    }

    // Log the success message through the client
    info!(
        "Structured log entry written successfully to: projects/{}/logs/{}",
        project_id, log_name
    );

    Ok(())
}

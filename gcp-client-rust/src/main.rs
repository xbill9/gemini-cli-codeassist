use anyhow::Context;
use google_cloud_gax::paginator::Paginator as _;
use google_cloud_secretmanager_v1::client::SecretManagerService;

pub type Result = std::result::Result<(), Box<dyn std::error::Error>>;

#[tokio::main]
async fn main() -> Result {
    // validate PROJECT_ID
    let project_id =
        std::env::var("PROJECT_ID").context("PROJECT_ID environment variable must be set")?;

    // Google Cloud Client API to secret manager
    let client = SecretManagerService::builder().build().await?;

    println!("Starting client API call Project {}", project_id);
    // make client call
    let mut items = client
        .list_locations()
        .set_name(format!("projects/{project_id}"))
        .by_page();
    while let Some(page) = items.next().await {
        let page = page?;
        for location in page.locations {
            println!("{}", location.name);
        }
    }

    println!("Completed client API call Project {}", project_id);

    // return OK
    Ok(())
}

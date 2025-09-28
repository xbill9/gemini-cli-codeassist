use anyhow::Result;
use weather_rust::run;

#[tokio::main]
async fn main() -> Result<()> {
    run().await
}

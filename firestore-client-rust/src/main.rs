use anyhow::{Context, Result};
use clap::Parser;
use firestore::FirestoreDb;
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::Arc;
use tracing::info;
use tracing_subscriber::{filter::EnvFilter, fmt, prelude::*};
use uuid::Uuid;

#[derive(Debug, Serialize, Deserialize)]
struct Invoice {
    first_name: String,
    last_name: String,
    email: String,
    product_id: String,
    qty: i64,
    amount: f64,
    invoice_date: String,
    address: String,
    city: String,
    stock_code: String,
    job: String,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Parser, Debug)]
enum Commands {
    /// Imports invoices from a CSV file into the Firestore database
    Invoices,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();

    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();

    let _ =
        rustls::crypto::CryptoProvider::install_default(rustls::crypto::ring::default_provider());

    let cli = Cli::parse();

    let gcp_project_id = std::env::var("PROJECT_ID").context(
        "PROJECT_ID environment variable not set. Please set it to your Google Cloud project ID.",
    )?;
    let db = Arc::new(FirestoreDb::new(&gcp_project_id).await?);

    match cli.command {
        Commands::Invoices => {
            info!("Importing invoices from invoices.csv...");
            let invoices = read_invoices_from_csv("invoices.csv")?;
            for invoice in invoices {
                add_invoice_to_firestore(&db, &invoice).await?;
            }
            info!("Invoices imported successfully.");
        }
    }
    Ok(())
}

async fn add_invoice_to_firestore(
    db: &Arc<FirestoreDb>,
    invoice: &Invoice,
) -> Result<(), firestore::errors::FirestoreError> {
    let invoice_id = Uuid::new_v4().to_string();
    db.fluent()
        .insert()
        .into("invoices")
        .document_id(&invoice_id)
        .object(invoice)
        .execute::<()>()
        .await?;
    Ok(())
}

fn read_invoices_from_csv<P: AsRef<Path>>(path: P) -> Result<Vec<Invoice>> {
    let mut rdr = csv::Reader::from_path(path)?;
    let mut invoices = Vec::new();
    for result in rdr.deserialize() {
        let invoice: Invoice = result?;
        invoices.push(invoice);
    }
    Ok(invoices)
}

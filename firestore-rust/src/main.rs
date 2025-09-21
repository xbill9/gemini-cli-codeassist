#![deny(warnings)]

use firestore::FirestoreDb;
use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use hyper::service::service_fn;
use hyper::{Method, Request, Response, StatusCode};
use hyper_util::rt::TokioExecutor;
use hyper_util::rt::TokioIo;
use hyper_util::server::conn::auto::Builder;
use serde::{Deserialize, Serialize};
use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::signal;
use tracing::{info, warn};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

use futures::stream::StreamExt;
use uuid::Uuid;

#[derive(Serialize, Deserialize, Debug, Clone)]
struct InventoryItem {
    #[serde(rename = "_firestore_id")]
    id: Option<String>,
    name: String,
    quantity: u32,
}

// An async function that responds to a health check
async fn health_check() -> Result<Response<Full<Bytes>>, Infallible> {
    Ok(Response::new(Full::new(Bytes::from("ok"))))
}

// An async function that adds an item to the inventory
async fn add_inventory(
    req: Request<impl hyper::body::Body>,
    db: Arc<FirestoreDb>,
) -> Result<Response<Full<Bytes>>, Infallible> {
    info!("Add inventory endpoint called");
    let collection_id = "inventory";

    let body_bytes = match req.into_body().collect().await {
        Ok(collected) => collected.to_bytes(),
        Err(_e) => {
            let mut response =
                Response::new(Full::new(Bytes::from("Error reading body".to_string())));
            *response.status_mut() = StatusCode::BAD_REQUEST;
            return Ok(response);
        }
    };

    let mut item: InventoryItem = match serde_json::from_slice(&body_bytes) {
        Ok(item) => item,
        Err(e) => {
            let mut response =
                Response::new(Full::new(Bytes::from(format!("Invalid JSON: {}", e))));
            *response.status_mut() = StatusCode::BAD_REQUEST;
            return Ok(response);
        }
    };

    item.id = Some(Uuid::new_v4().to_string());

    let result = db
        .fluent()
        .insert()
        .into(collection_id)
        .document_id(item.id.as_ref().unwrap())
        .object(&item)
        .execute::<InventoryItem>()
        .await;

    match result {
        Ok(doc) => Ok(Response::new(Full::new(Bytes::from(format!(
            "Document created: {}",
            doc.name
        ))))),
        Err(e) => Ok(Response::new(Full::new(Bytes::from(format!(
            "Error creating document: {}",
            e
        ))))),
    }
}

// An async function that lists all items in the inventory
async fn get_inventory(db: Arc<FirestoreDb>) -> Result<Response<Full<Bytes>>, Infallible> {
    info!("Get inventory endpoint called");
    let collection_id = "inventory";

    let object_stream = db
        .fluent()
        .select()
        .from(collection_id)
        .obj::<InventoryItem>()
        .stream_query()
        .await;

    match object_stream {
        Ok(stream) => {
            let items: Vec<InventoryItem> = stream.collect().await;
            let json_body = serde_json::to_string(&items)
                .unwrap_or_else(|e| format!("Error serializing inventory: {}", e));

            let mut response = Response::new(Full::new(Bytes::from(json_body)));
            response.headers_mut().insert(
                hyper::header::CONTENT_TYPE,
                hyper::header::HeaderValue::from_static("application/json"),
            );
            Ok(response)
        }
        Err(e) => {
            let mut response = Response::new(Full::new(Bytes::from(format!(
                "Error listing documents: {}",
                e
            ))));
            *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
            Ok(response)
        }
    }
}

// Router function to handle different routes
async fn router(
    req: Request<impl hyper::body::Body>,
    db: Arc<FirestoreDb>,
) -> Result<Response<Full<Bytes>>, Infallible> {
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/health") => health_check().await,
        (&Method::POST, "/products") => add_inventory(req, db).await,
        (&Method::GET, "/products") => get_inventory(db).await,
        _ => {
            let mut not_found = Response::new(Full::new(Bytes::from("Not Found")));
            *not_found.status_mut() = StatusCode::NOT_FOUND;
            Ok(not_found)
        }
    }
}

// A signal handler for graceful shutdown.
async fn shutdown_signal() {
    let ctrl_c = async {
        signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }

    info!("Signal received, starting graceful shutdown");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Initialize tracing subscriber for logging
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();

    // Initialize Firestore client
    let client =
        FirestoreDb::new(&std::env::var("PROJECT_ID").expect("PROJECT_ID not set")).await?;
    let db = Arc::new(client);

    // Determine the port to listen on, defaulting to 8080
    let port = std::env::var("PORT").unwrap_or_else(|_| "8080".to_string());
    let addr: SocketAddr = format!("0.0.0.0:{}", port)
        .parse()
        .expect("Invalid address format");

    // Create a TcpListener and bind it to the address
    let listener = TcpListener::bind(addr).await?;
    info!("Listening on http://{}", addr);

    // Run the server with graceful shutdown
    loop {
        let db = db.clone();
        let (stream, _) = tokio::select! {
            res = listener.accept() => {
                match res {
                    Ok(conn) => conn,
                    Err(e) => {
                        warn!("error accepting connection: {}", e);
                        continue;
                    }
                }
            }
            _ = shutdown_signal() => {
                info!("shutting down");
                break;
            }
        };

        let io = TokioIo::new(stream);

        tokio::task::spawn(async move {
            if let Err(err) = Builder::new(TokioExecutor::new())
                .serve_connection(
                    io,
                    service_fn(move |req| {
                        let db = db.clone();
                        router(req, db)
                    }),
                )
                .await
            {
                warn!("server error: {}", err);
            }
        });
    }

    Ok(())
}

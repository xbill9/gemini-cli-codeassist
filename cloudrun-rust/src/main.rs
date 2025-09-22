#![deny(warnings)]

//! A simple "Hello, World!" HTTP server using hyper.
//!
//! This server listens on the port specified by the `PORT` environment variable,
//! or 8080 by default. It gracefully shuts down on `Ctrl+C` or `SIGTERM`.

use http_body_util::Full;
use hyper::body::Bytes;
use hyper::service::service_fn;
use hyper::{Request, Response};
use hyper_util::rt::TokioExecutor;
use hyper_util::rt::TokioIo;
use hyper_util::server::conn::auto::Builder;
use std::convert::Infallible;
use std::net::SocketAddr;
use tokio::net::TcpListener;
use tokio::signal;
use tracing::{error, info, warn};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Handles incoming HTTP requests and returns a "Hello, World!" response.
///
/// This function is called for every incoming request. It ignores the request
/// body and method, and always returns a successful response with a fixed string.
async fn hello(_: Request<impl hyper::body::Body>) -> Result<Response<Full<Bytes>>, Infallible> {
    Ok(Response::new(Full::new(Bytes::from("Hello, Rust!!!"))))
}

/// Listens for shutdown signals (`Ctrl+C` or `SIGTERM`) to enable graceful shutdown.
///
/// This function will block until a shutdown signal is received.
/// On Unix systems, it listens for both `Ctrl+C` and `SIGTERM`.
/// On other systems, it only listens for `Ctrl+C`.
async fn shutdown_signal() {
    let ctrl_c = async {
        if let Err(e) = signal::ctrl_c().await {
            warn!("Failed to listen for Ctrl+C: {}", e);
            // Pend forever if the handler fails to prevent the server from shutting down.
            std::future::pending().await
        }
    };

    #[cfg(unix)]
    let terminate = async {
        match signal::unix::signal(signal::unix::SignalKind::terminate()) {
            Ok(mut stream) => {
                stream.recv().await;
            }
            Err(e) => {
                warn!("Failed to listen for SIGTERM: {}", e);
                // Pend forever if the handler fails.
                std::future::pending().await
            }
        }
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }

    info!("Signal received, starting graceful shutdown");
}

/// The main entry point for the application.
///
/// This function initializes the logger, reads configuration, binds to a TCP socket,
/// and runs the main server loop, handling incoming connections and graceful shutdown.
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Initialize tracing subscriber for structured logging.
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();

    // Determine the port to listen on from the `PORT` environment variable,
    // defaulting to 8080 if not set.
    let port: u16 = std::env::var("PORT")
        .unwrap_or_else(|_| "8080".to_string())
        .parse()?;
    let addr = SocketAddr::from(([0, 0, 0, 0], port));

    // Create a TcpListener and bind it to the address.
    let listener = TcpListener::bind(addr).await?;

    // The main server loop.
    // This loop continuously accepts new connections and spawns a task to handle them.
    // It also listens for a shutdown signal to exit gracefully.
    loop {
        let (stream, _) = tokio::select! {
            // Accept a new incoming connection.
            res = listener.accept() => {
                match res {
                    Ok(conn) => conn,
                    Err(e) => {
                        // Log the error and continue to the next iteration.
                        error!("error accepting connection: {}", e);
                        continue;
                    }
                }
            }
            // Wait for the shutdown signal.
            _ = shutdown_signal() => {
                info!("shutting down");
                break;
            }
        };

        // Use TokioIo to adapt the TcpStream for hyper.
        let io = TokioIo::new(stream);

        // Spawn a new asynchronous task to handle the connection.
        tokio::task::spawn(async move {
            // Use hyper's auto builder to serve the connection.
            // The `service_fn` creates a service from our `hello` async function.
            if let Err(err) = Builder::new(TokioExecutor::new())
                .serve_connection(io, service_fn(hello))
                .await
            {
                warn!("server error: {}", err);
            }
        });
    }

    Ok(())
}

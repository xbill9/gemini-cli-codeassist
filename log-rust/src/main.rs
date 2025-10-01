use anyhow::Result;
use reqwest::Client;
use rmcp::{
    handler::server::{ServerHandler, tool::ToolRouter},
    model::{ServerCapabilities, ServerInfo},
    transport::streamable_http_server::{session::local::LocalSessionManager, StreamableHttpService},
    tool,
    tool_router,
    tool_handler,
    schemars,
};

use cloud_profiler_rust::CloudProfilerConfiguration;
use rmcp::handler::server::wrapper::Parameters;
use opentelemetry::trace::TracerProvider;
use tracing_opentelemetry::OpenTelemetryLayer;




use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

const NWS_API_BASE: &str = "https://api.weather.gov";
const USER_AGENT: &str = "weather-app/2.0";
const BIND_ADDRESS: &str = "0.0.0.0:8080";

#[derive(Debug, serde::Deserialize)]
struct AlertResponse {
    pub features: Vec<Feature>,
}

#[derive(Debug, serde::Deserialize)]
struct Feature {
    pub properties: FeatureProps,
}

#[derive(Debug, serde::Deserialize)]
struct FeatureProps {
    pub event: String,
    #[serde(rename = "areaDesc")]
    pub area_desc: String,
    pub severity: String,
    pub status: String,
    pub headline: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct PointsResponse {
    pub properties: PointsProps,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct PointsProps {
    pub forecast: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GridPointsResponse {
    pub properties: GridPointsProps,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GridPointsProps {
    pub periods: Vec<Period>,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct Period {
    pub name: String,
    pub temperature: i32,
    #[serde(rename = "temperatureUnit")]
    pub temperature_unit: String,
    #[serde(rename = "windSpeed")]
    pub wind_speed: String,
    #[serde(rename = "windDirection")]
    pub wind_direction: String,
    #[serde(rename = "shortForecast")]
    pub short_forecast: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GetAlertsRequest {
    #[schemars(description = "the US state to get alerts for")]
    pub state: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
struct GetForecastRequest {
    #[schemars(description = "latitude of the location in decimal format")]
    pub latitude: String,
    #[serde(rename = "longitude")]
    pub longitude: String,
}

fn format_alerts(alerts: &[Feature]) -> String {
    if alerts.is_empty() {
        "No active alerts found.".to_string()
    } else {
        alerts
            .iter()
            .map(|alert| {
                format!(
                    "Event: {}\nArea: {}\nSeverity: {}\nStatus: {}\nHeadline: {}",
                    alert.properties.event,
                    alert.properties.area_desc,
                    alert.properties.severity,
                    alert.properties.status,
                    alert.properties.headline
                )
            })
            .collect::<Vec<String>>()
            .join("\n---\n")
    }
}

fn format_forecast(periods: &[Period]) -> String {
    if periods.is_empty() {
        "No forecast data available.".to_string()
    } else {
        periods
            .iter()
            .map(|period| {
                format!(
                    "Name: {}\nTemperature: {}°{}\nWind: {} {}\nForecast: {}",
                    period.name,
                    period.temperature,
                    period.temperature_unit,
                    period.wind_speed,
                    period.wind_direction,
                    period.short_forecast
                )
            })
            .collect::<Vec<String>>()
            .join("\n---\n")
    }
}

fn log_and_format_error(context: &str, e: &anyhow::Error) -> String {
    tracing::error!("Failed to {}: {:?}", context, e);
    format!("Failed to {}: {}", context, e)
}

#[derive(Debug, Clone)]
struct Weather {
    tool_router: ToolRouter<Self>,
    client: Client,
}

#[tool_router]
impl Weather {
    fn new() -> anyhow::Result<Self> {
        Ok(Self {
            tool_router: Self::tool_router(),
            client: reqwest::Client::builder().user_agent(USER_AGENT).build()?,
        })
    }

    async fn make_request<T>(&self, url: &str) -> anyhow::Result<T>
    where
        T: serde::de::DeserializeOwned,
    {
        tracing::info!("Making request to: {}", url);

        let response = self
            .client
            .get(url)
            .send()
            .await
            .map_err(|e| anyhow::anyhow!("Request failed: {}", e))?;

        tracing::info!("Received response: {:?}", response);

        let status = response.status();
        if status.is_success() {
            response
                .json::<T>()
                .await
                .map_err(|e| anyhow::anyhow!("Failed to parse response: {}", e))
        } else {
            Err(anyhow::anyhow!("Request failed with status: {}", status))
        }
    }

    #[tool(description = "Get weather alerts for a US state")]
    async fn get_alerts(
        &self,
        Parameters(GetAlertsRequest { state }): Parameters<GetAlertsRequest>,
    ) -> String {
        tracing::info!("Received request for weather alerts in state: {}", state);

        let url = format!("{}/alerts/active?area={}", NWS_API_BASE, state);

        match self.make_request::<AlertResponse>(&url).await {
            Ok(alerts) => {
                tracing::debug!("Found {} alerts", alerts.features.len());
                format_alerts(&alerts.features)
            }
            Err(e) => log_and_format_error("fetch alerts", &e),
        }
    }

    #[tool(description = "Get forecast using latitude and longitude coordinates")]
    async fn get_forecast(
        &self,
        Parameters(GetForecastRequest {
            latitude,
            longitude,
        }): Parameters<GetForecastRequest>,
    ) -> String {
        tracing::info!(
            "Received coordinates: latitude = {}, longitude = {}",
            latitude,
            longitude
        );

        let points_url = format!("{}/points/{},{}", NWS_API_BASE, latitude, longitude);

        match self.make_request::<PointsResponse>(&points_url).await {
            Ok(points) => {
                tracing::debug!("Got forecast URL: {}", points.properties.forecast);
                // Get the forecast data
                match self
                    .make_request::<GridPointsResponse>(&points.properties.forecast)
                    .await
                {
                    Ok(forecast) => {
                        tracing::debug!(
                            "Found {} forecast periods",
                            forecast.properties.periods.len()
                        );
                        format_forecast(&forecast.properties.periods)
                    }
                    Err(e) => log_and_format_error("fetch forecast", &e),
                }
            }
            Err(e) => log_and_format_error("fetch points", &e),
        }
    }
}

#[tool_handler]
impl ServerHandler for Weather {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some("A simple weather forecaster".into()),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

async fn run() -> anyhow::Result<()> {
    let subscriber = tracing_subscriber::registry().with(
        tracing_subscriber::EnvFilter::try_from_default_env()
            .unwrap_or_else(|_| "info,min_rust=debug".into()),
    );

    if let Ok(project_id) = std::env::var("GCP_PROJECT_ID") {
        // We are running on Google Cloud, so we can initialize the layers.
        let provider = opentelemetry_gcloud_trace::GcpCloudTraceExporterBuilder::new(project_id.clone())
            .create_provider()
            .await?;
        let tracer = provider.tracer("tracing");
        let trace_layer = OpenTelemetryLayer::new(tracer);

        // Initialize Profiling
        tracing::info!("Initializing Google Cloud Profiler (unofficial crate).");
        tokio::spawn(async move {
            let _ = cloud_profiler_rust::maybe_start_profiling(
                project_id,
                "log-rust".to_string(),
                "0.1.0".to_string(),
                || true,
                || CloudProfilerConfiguration { sampling_rate: 1 },
            )
            .await;
        });

        tracing::info!("Google Cloud Tracing and Profiling are enabled.");
        tracing::info!("Note: There is no Google Cloud Debugger for Rust.");

        subscriber.with(trace_layer).init();
    } else {
        // We are not running on Google Cloud, so we will not initialize the layers.
        tracing::info!("Not running on Google Cloud, so Google Cloud Observability is disabled.");
        subscriber
            .with(tracing_subscriber::fmt::layer().pretty())
            .init();
    };

    tracing::info!("Starting server on {}", BIND_ADDRESS);

    let service = StreamableHttpService::new(
        || Weather::new().map_err(|e| std::io::Error::other(e.to_string())),
        LocalSessionManager::default().into(),
        Default::default(),
    );

    let router = axum::Router::new().nest_service("/mcp", service);
    let tcp_listener = tokio::net::TcpListener::bind(BIND_ADDRESS).await?;

    tracing::info!("Server listening on {}", BIND_ADDRESS);

    let _ = axum::serve(tcp_listener, router)
        .with_graceful_shutdown(async {
            if let Err(e) = tokio::signal::ctrl_c().await {
                tracing::error!("Failed to listen for ctrl-c: {}", e)
            }
            tracing::info!("Shutting down...");
        })
        .await;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    run().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_alerts_no_alerts() {
        let alerts = vec![];
        let formatted_alerts = format_alerts(&alerts);
        assert_eq!(formatted_alerts, "No active alerts found.");
    }

    #[test]
    fn test_format_alerts_single_alert() {
        let alerts = vec![Feature {
            properties: FeatureProps {
                event: "Tornado Warning".to_string(),
                area_desc: "Someplace, USA".to_string(),
                severity: "Extreme".to_string(),
                status: "Actual".to_string(),
                headline: "Tornado Warning issued for Someplace, USA".to_string(),
            },
        }];
        let formatted_alerts = format_alerts(&alerts);
        let expected = "Event: Tornado Warning\nArea: Someplace, USA\nSeverity: Extreme\nStatus: Actual\nHeadline: Tornado Warning issued for Someplace, USA";
        assert_eq!(formatted_alerts, expected);
    }

    #[test]
    fn test_format_forecast_no_periods() {
        let periods = vec![];
        let formatted_forecast = format_forecast(&periods);
        assert_eq!(formatted_forecast, "No forecast data available.");
    }

    #[test]
    fn test_format_forecast_single_period() {
        let periods = vec![Period {
            name: "Today".to_string(),
            temperature: 70,
            temperature_unit: "F".to_string(),
            wind_speed: "10 mph".to_string(),
            wind_direction: "S".to_string(),
            short_forecast: "Sunny".to_string(),
        }];
        let formatted_forecast = format_forecast(&periods);
        let expected = "Name: Today\nTemperature: 70°F\nWind: 10 mph S\nForecast: Sunny";
        assert_eq!(formatted_forecast, expected);
    }
}
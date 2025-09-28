use weather_rust::*;

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
    let expected = "Event: Tornado Warning\nArea: Someplace, USA\nSeverity: Extreme\nStatus: Actual\nHeadline: Tornado Warning issued for Someplace, USA\n---\n";
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
    let expected = "Name: Today\nTemperature: 70°F\nWind: 10 mph S\nForecast: Sunny\n---\n";
    assert_eq!(formatted_forecast, expected);
}

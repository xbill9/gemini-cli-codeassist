use anyhow::Result;
use clap::Parser;
use rmcp::{
    model::{CallToolRequestParam, CallToolResult, RawContent},
    transport::TokioChildProcess,
    ServiceExt,
};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Command to start the MCP server
    server_cmd: String,

    /// Arguments for the MCP server
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    server_args: Vec<String>,
}

fn init_tracing() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info".into()),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(std::io::stderr)
                .pretty(),
        )
        .init();
}

fn extract_text_from_result(result: CallToolResult) -> String {
    result
        .content
        .first()
        .and_then(|annotated| match &annotated.raw {
            RawContent::Text(raw_text_content) => Some(raw_text_content.text.clone()),
            _ => None,
        })
        .unwrap_or_else(|| "(No text content found)".to_string())
}

fn parse_json_args(json_str: &str) -> Result<serde_json::Map<String, serde_json::Value>, serde_json::Error> {
    match serde_json::from_str(json_str) {
        Ok(serde_json::Value::Object(map)) => Ok(map),
        Ok(_) => {
             // Return an error that indicates it wasn't an object
             // Using a trick to create a synthetic error or just mapping to a custom error would be better
             // but for now, we rely on serde's error handling.
             // We can't easily construct a serde_json::Error from scratch.
             // So we'll try to deserialize as an object specifically to trigger the error.
             serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(json_str).map_err(|e| e)
        },
        Err(e) => Err(e),
    }
}


#[tokio::main]
async fn main() -> Result<()> {
    init_tracing();
    let args = Args::parse();

    let server_cmd = &args.server_cmd;
    let server_args = &args.server_args;

    tracing::info!("Starting MCP Client connecting to: {} {:?}", server_cmd, server_args);

    let mut cmd = tokio::process::Command::new(server_cmd);
    cmd.args(server_args)
        .kill_on_drop(true)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit());

    // Create a new client that communicates with an MCP server
    // launched as a child process.
    let client = ().serve(TokioChildProcess::new(cmd)?).await?;

    println!("Connected to MCP Server.");
    println!("Type 'get_tools' to see tools, 'exec <tool> <args_json>' to call a tool, or 'quit' to exit.");

    let mut rl = DefaultEditor::new()?;

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(line);

                let parts = match shell_words::split(line) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("Parse error: {}", e);
                        continue;
                    }
                };
                
                if parts.is_empty() { continue; }

                match parts[0].as_str() {
                    "quit" | "exit" => break,
                    "get_tools" => {
                        match client.list_tools(None).await {
                            Ok(result) => {
                                println!("Tools:");
                                for tool in result.tools {
                                    println!("- {}: {}", tool.name, tool.description.unwrap_or_default());
                                    println!("  Input Schema: {}", serde_json::to_string_pretty(&tool.input_schema).unwrap_or_default());
                                }
                            }
                            Err(e) => eprintln!("Error listing tools: {:?}", e),
                        }
                    }
                    "exec" => {
                        if parts.len() < 2 {
                            eprintln!("Usage: call <tool_name> [json_args]");
                            continue;
                        }
                        let tool_name = &parts[1];
                        let json_args_str = if parts.len() > 2 {
                             &parts[2]
                        } else {
                            "{}"
                        };

                        let args_map = match parse_json_args(json_args_str) {
                            Ok(map) => map,
                            Err(e) => {
                                eprintln!("Invalid JSON args (must be an object): {}", e);
                                continue;
                            }
                        };

                        let req = CallToolRequestParam {
                            name: tool_name.to_string().into(),
                            arguments: Some(args_map),
                        };

                        tracing::info!("Calling tool '{}'...", tool_name);
                        match client.call_tool(req).await {
                            Ok(res) => {
                                let text = extract_text_from_result(res);
                                println!("Result: {}", text);
                            }
                            Err(e) => eprintln!("Error calling tool: {:?}", e),
                        }
                    }
                    _ => println!("Unknown command. Available: get_tools, exec, quit"),
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rmcp::model::{Annotated, RawContent, RawTextContent};

    #[test]
    fn test_extract_text_from_result_success() {
        let text_content = RawTextContent {
            text: "Hello World".to_string(),
            meta: None,
        };
        let annotated = Annotated {
            raw: RawContent::Text(text_content),
            annotations: None,
        };
        let result = CallToolResult {
            content: vec![annotated],
            is_error: None,
            meta: None,
            structured_content: None,
        };

        assert_eq!(extract_text_from_result(result), "Hello World");
    }

    #[test]
    fn test_extract_text_from_result_empty() {
        let result = CallToolResult {
            content: vec![],
            is_error: None,
            meta: None,
            structured_content: None,
        };

        assert_eq!(extract_text_from_result(result), "(No text content found)");
    }

    #[test]
    fn test_parse_json_args_valid() {
        let json = "{\"arg1\": \"value1\", \"arg2\": 2}";
        let result = parse_json_args(json);
        assert!(result.is_ok());
        let map = result.unwrap();
        assert_eq!(map["arg1"], "value1");
        assert_eq!(map["arg2"], 2);
    }

    #[test]
    fn test_parse_json_args_empty_object() {
        let json = "{}";
        let result = parse_json_args(json);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_parse_json_args_invalid_json() {
        let json = "{invalid";
        let result = parse_json_args(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_json_args_not_object() {
        let json = "[]";
        let result = parse_json_args(json);
        assert!(result.is_err());
        
        let json = "\"string\"";
        let result = parse_json_args(json);
        assert!(result.is_err());
    }
}
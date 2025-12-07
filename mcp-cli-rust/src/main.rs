use anyhow::Result;
use rmcp::{
    model::{CallToolRequestParam, CallToolResult},
    transport::TokioChildProcess,
    ServiceExt,
};
use std::io::{self, Write};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

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
            rmcp::model::RawContent::Text(raw_text_content) => Some(raw_text_content.text.clone()),
            _ => None,
        })
        .unwrap_or_else(|| "(No text content found)".to_string())
}

#[tokio::main]
async fn main() -> Result<()> {
    init_tracing();

    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <server_command> [server_args...]", args[0]);
        return Ok(());
    }

    let server_cmd = &args[1];
    let server_args = &args[2..];

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
    println!("Type 'list' to see tools, 'call <tool> <args_json>' to call a tool, or 'quit' to exit.");

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            break;
        }

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        let parts: Vec<&str> = input.split_whitespace().collect();
        match parts[0] {
            "quit" | "exit" => break,
            "list" => {
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
            "call" => {
                if parts.len() < 2 {
                    eprintln!("Usage: call <tool_name> [json_args]");
                    continue;
                }
                let tool_name = parts[1];
                
                // Reconstruct the rest of the string as JSON args
                let json_args_str = if parts.len() > 2 {
                    // Find where the second argument starts in the original input
                    // This is a bit rough but works for simple cases. 
                    // Better would be to join the rest of the parts, but that loses spacing inside JSON.
                    // We'll look for the first occurrence of parts[2] after parts[1].
                    let remainder_start = input.find(parts[1]).unwrap() + parts[1].len();
                    input[remainder_start..].trim()
                } else {
                    "{}"
                };

                let args_map: serde_json::Map<String, serde_json::Value> = match serde_json::from_str(json_args_str) {
                    Ok(serde_json::Value::Object(map)) => map,
                    Ok(_) => {
                        eprintln!("Arguments must be a JSON object (e.g., {{ \"arg\": \"value\" }})");
                        continue;
                    },
                    Err(e) => {
                        eprintln!("Invalid JSON args: {}", e);
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
            _ => println!("Unknown command. Available: list, call, quit"),
        }
    }

    Ok(())
}

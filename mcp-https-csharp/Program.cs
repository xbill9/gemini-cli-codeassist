using ModelContextProtocol.AspNetCore;
using ModelContextProtocol.Server;
using System.ComponentModel;

var builder = WebApplication.CreateBuilder(args);

// Configure logging to stderr
builder.Logging.AddConsole(options =>
{
    options.LogToStandardErrorThreshold = LogLevel.Trace;
});

// Add MCP server capabilities and automatically register tools
builder.Services.AddMcpServer()
    .WithHttpTransport()
    .WithToolsFromAssembly(); // Scans assembly for tool definitions

var app = builder.Build();

app.Logger.LogInformation("MCP Server starting on http://0.0.0.0:8080");

// Map the MCP endpoint
app.MapMcp("/mcp");

app.Run("http://0.0.0.0:8080");


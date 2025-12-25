using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using ModelContextProtocol.Server;
using System.ComponentModel;
using System.Threading.Tasks;

[McpServerToolType]
public static class MyTools
{
    [McpServerTool]
    public static string Greet(string name)
    {
        return $"Hello, {name}!";
    }
}

public class Program
{
    public static async Task Main(string[] args)
    {
        var builder = Host.CreateApplicationBuilder(args);

        builder.Logging.AddConsole(consoleLogOptions =>
        {
            // Configure all logs to go to stderr
            consoleLogOptions.LogToStandardErrorThreshold = LogLevel.Trace;
        });

        builder.Services.AddMcpServer()
            .WithStdioServerTransport()
            .WithToolsFromAssembly();

        using IHost host = builder.Build();
        await host.RunAsync();
    }
}

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

    [McpServerTool]
    [Description("Returns the current system time.")]
    public static string GetTime()
    {
        return System.DateTime.Now.ToString();
    }

    [McpServerTool]
    [Description("Returns system specifications and information.")]
    public static string GetSystemInfo()
    {
        var os = System.Runtime.InteropServices.RuntimeInformation.OSDescription;
        var arch = System.Runtime.InteropServices.RuntimeInformation.OSArchitecture;
        var framework = System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription;
        var processorCount = System.Environment.ProcessorCount;
        var machineName = System.Environment.MachineName;

        return $"OS: {os}\nArchitecture: {arch}\nFramework: {framework}\nProcessor Count: {processorCount}\nMachine Name: {machineName}";
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

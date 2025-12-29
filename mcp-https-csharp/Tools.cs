using ModelContextProtocol.Server;
using System.ComponentModel;

namespace mcp_https_csharp;

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

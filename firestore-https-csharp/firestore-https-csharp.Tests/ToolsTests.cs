using Xunit;
using firestore_https_csharp;
using System;

namespace firestore_https_csharp.Tests;

public class ToolsTests
{
    [Fact]
    public void Greet_ReturnsGreeting()
    {
        var result = MyTools.Greet("World");
        Assert.Equal("Hello, World!", result);
    }

    [Fact]
    public void GetTime_ReturnsValidTime()
    {
        var result = MyTools.GetTime();
        Assert.True(DateTime.TryParse(result, out _));
    }

    [Fact]
    public void GetSystemInfo_ReturnsInfo()
    {
        var result = MyTools.GetSystemInfo();
        Assert.Contains("OS:", result);
        Assert.Contains("Architecture:", result);
        Assert.Contains("Framework:", result);
        Assert.Contains("Processor Count:", result);
        Assert.Contains("Machine Name:", result);
    }
}

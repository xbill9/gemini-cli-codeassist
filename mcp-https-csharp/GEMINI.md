# Gemini Code Assistant Context

This document provides context for the Gemini Code Assistant to understand the project and assist in development.

## Project Overview

This is a **C#-based Model Context Protocol (MCP) server** using the `ModelContextProtocol` and `ModelContextProtocol.AspNetCore` libraries. It is designed to expose tools (like `greet`) over **HTTPS/SSE** for integration with MCP clients.

## Key Technologies

*   **Language:** C#
*   **Framework:** .NET 10 (ASP.NET Core)
*   **SDK:** `ModelContextProtocol.AspNetCore`
*   **Dependency Management:** NuGet

## Project Structure

*   `Program.cs`: The entry point of the application. Initializes the MCP server using ASP.NET Core and defines tools.
*   `mcp-https-csharp.csproj`: C# project file (renamed from mcp-stdio-csharp.csproj).
*   `Makefile`: Development shortcuts (restore, build, run, test, format, lint, clean).

## Development Setup

1.  **Install .NET SDK:** Ensure the .NET 8 SDK is installed on your system.

2.  **Restore Dependencies:**
    ```bash
    dotnet restore
    ```

## Running the Server

The server is configured to run using the `https` transport.

```bash
dotnet run
```

*Note: Since this is an MCP server running over stdio, it is typically not run directly by a human but rather spawned by an MCP client.*

## C# MCP Developer Resources

*   **MCP C# SDK (GitHub):** [https://github.com/modelcontextprotocol/csharp-sdk](https://github.com/modelcontextprotocol/csharp-sdk)
*   **`ModelContextProtocol` package on NuGet:** [https://www.nuget.org/packages/ModelContextProtocol](https://www.nuget.org/packages/ModelContextProtocol)

## Legacy/mismatched files
*   `Dockerfile`: Needs update for C#/.NET environment.
*   `cloudbuild.yaml`: Needs update for C#/.NET builds.

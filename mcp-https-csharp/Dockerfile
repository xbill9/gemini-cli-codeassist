# Use Microsoft's official .NET image.
# https://hub.docker.com/_/microsoft-dotnet-sdk/
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /app

# Copy csproj and restore as distinct layers
COPY mcp-https-csharp.csproj .
RUN dotnet restore

# Copy everything else and build
COPY . .
RUN dotnet publish -c Release -o /app/publish

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:10.0
WORKDIR /app
COPY --from=build /app/publish .

# Set environment variables
ENV PORT=8080
# ASP.NET Core specific environment variable to listen on the correct port
ENV ASPNETCORE_URLS=http://0.0.0.0:8080

EXPOSE 8080

# Start the application
ENTRYPOINT ["dotnet", "mcp-https-csharp.dll"]
package main

import (
	"context"
	"log/slog"
	"os"
	"os/signal"
	"syscall"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

type Input struct {
	Name string `json:"name" jsonschema:"the name of the person to greet"`
}

type Output struct {
	Greeting string `json:"greeting" jsonschema:"the greeting to tell to the user"`
}

func SayHi(ctx context.Context, req *mcp.CallToolRequest, input Input) (
	*mcp.CallToolResult,
	Output,
	error,
) {
	slog.Info("Received SayHi request", "name", input.Name)
	output := Output{Greeting: "Hi " + input.Name}
	slog.Info("Returning greeting", "greeting", output.Greeting)
	return nil, output, nil
}

func main() {
	// Create a server with a single tool.
	server := mcp.NewServer(&mcp.Implementation{Name: "greeter", Version: "v1.0.0"}, nil)
	mcp.AddTool(server, &mcp.Tool{Name: "greet", Description: "say hi"}, SayHi)

	// Handle graceful shutdown
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	// Run the server over stdin/stdout, until the client disconnects.
	slog.Info("Starting greeter service...")
	if err := server.Run(ctx, &mcp.StdioTransport{}); err != nil {
		slog.Error("Server exit", "error", err)
		os.Exit(1)
	}
}
import 'dart:io';
import 'package:mcp_dart/mcp_dart.dart';
import 'package:mcp_stdio_flutter/tools.dart';

void main() async {
  // Set up logging
  void log(String message) {
    stderr.writeln(message);
  }

  final server = McpServer(
    Implementation(
      name: "mcp-stdio-flutter",
      version: "1.0.0",
    ),
    options: McpServerOptions(
      capabilities: ServerCapabilities(
        tools: ServerCapabilitiesTools(listChanged: true),
      ),
    ),
  );

  server.registerTool(
    "greet",
    description: "Get a greeting from a local stdio server.",
    inputSchema: JsonSchema.object(
      properties: {
        "param": JsonSchema.string(description: "The parameter to return."),
      },
      required: ["param"],
    ),
    callback: (args, extra) async {
       log("Executed greet tool");
       return greetHandler(args, extra);
    },
  );

  final transport = StdioServerTransport();
  await server.connect(transport);
}

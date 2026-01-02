import 'dart:io';
import 'package:mcp_dart/mcp_dart.dart';
import 'package:mcp_https_flutter/tools.dart';
import 'package:args/args.dart';

McpServer createServer() {
  final server = McpServer(
    Implementation(
      name: "mcp-https-flutter",
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
    description: "Get a greeting from a local server.",
    inputSchema: JsonSchema.object(
      properties: {
        "param": JsonSchema.string(description: "The parameter to return."),
      },
      required: ["param"],
    ),
    callback: (args, extra) async {
      stderr.writeln("Executed greet tool");
      return greetHandler(args, extra);
    },
  );

  return server;
}

void main(List<String> arguments) async {
  final parser = ArgParser()
    ..addOption('transport',
        abbr: 't',
        allowed: ['stdio', 'http'],
        defaultsTo: 'stdio',
        help: 'Transport to use')
    ..addOption('port',
        abbr: 'p', defaultsTo: '8080', help: 'Port for HTTP transport')
    ..addOption('host', defaultsTo: 'localhost', help: 'Host for HTTP transport')
    ..addOption('path', defaultsTo: '/mcp', help: 'Path for HTTP transport');

  final results = parser.parse(arguments);
  final transportType = results['transport'];

  if (transportType == 'stdio') {
    final server = createServer();
    final transport = StdioServerTransport();
    await server.connect(transport);
    stderr.writeln("Connected via stdio");
  } else {
    final port = int.parse(results['port']);
    final host = results['host'];
    final path = results['path']!;

    final server = StreamableMcpServer(
      serverFactory: (sessionId) => createServer(),
      host: host,
      port: port,
      path: path,
    );
    await server.start();
  }
}
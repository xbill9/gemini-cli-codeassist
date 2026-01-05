import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:args/args.dart';
import 'package:logging/logging.dart' as dart_logging;
import 'package:mcp_dart/mcp_dart.dart';
import 'package:mcp_https_flutter/mcp_https_flutter.dart';

void _setupLogging() {
  // Configure standard Dart logging
  dart_logging.Logger.root.level = dart_logging.Level.ALL;
  dart_logging.Logger.root.onRecord.listen((record) {
    final logEntry = {
      'timestamp': record.time.toUtc().toIso8601String(),
      'level': record.level.name,
      'logger': record.loggerName,
      'message': record.message,
      if (record.error != null) 'error': record.error.toString(),
      if (record.stackTrace != null) 'stackTrace': record.stackTrace.toString(),
    };
    stderr.writeln(jsonEncode(logEntry));
  });

  // Configure mcp_dart internal logging
  Logger.setHandler((loggerName, level, message) {
    final logEntry = {
      'timestamp': DateTime.now().toUtc().toIso8601String(),
      'level': level.name.toUpperCase(),
      'logger': loggerName,
      'message': message,
    };
    stderr.writeln(jsonEncode(logEntry));
  });
}

final logger = dart_logging.Logger('mcp_server');

/// Creates and configures an MCP server with a 'greet' tool.
McpServer createServer() {
  final server = McpServer(
    const Implementation(
      name: "mcp-https-flutter",
      version: "1.0.0",
    ),
    options: const McpServerOptions(
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
      logger.info("Executing greet tool with args: $args");
      return greetHandler(args, extra);
    },
  );

  return server;
}

/// Entry point for the MCP HTTPS Flutter server.
void main(List<String> arguments) async {
  _setupLogging();

  await runZoned(
    () async {
      final parser = ArgParser()
        ..addOption(
          'port',
          abbr: 'p',
          defaultsTo: '8080',
          help: 'Port for HTTP transport',
        )
        ..addOption(
          'host',
          defaultsTo: '0.0.0.0',
          help: 'Host for HTTP transport',
        )
        ..addOption('path', defaultsTo: '/mcp', help: 'Path for HTTP transport');

      final results = parser.parse(arguments);
      final portStr = results['port'] as String;
      final port = int.tryParse(portStr);
      if (port == null) {
        logger.severe("Invalid port number: $portStr");
        exit(1);
      }
      final host = results['host'] as String;
      final path = results['path'] as String;

      final server = StreamableMcpServer(
        serverFactory: (sessionId) => createServer(),
        host: host,
        port: port,
        path: path,
      );
      logger.info("Starting MCP server on http://$host:$port$path");
      await server.start();
    },
    zoneSpecification: ZoneSpecification(
      print: (self, parent, zone, line) {
        // Redirect all print calls to our structured logger
        logger.info(line);
      },
    ),
  );
}

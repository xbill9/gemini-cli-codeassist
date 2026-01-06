import 'package:firestore_https_flutter/tools.dart';
import 'package:firestore_https_flutter/logger.dart';
import 'package:args/args.dart';
import 'package:firedart/firedart.dart';
import 'package:dotenv/dotenv.dart';

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
        "name": JsonSchema.string(description: "The name to greet."),
      },
      required: ["name"],
    ),
    callback: (args, extra) async {
      log('INFO', 'Executed greet tool', {'args': args});
      return greetHandler(args, extra);
    },
  );

  server.registerTool(
    "get_products",
    description: "Get a list of all products from the inventory database",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log('INFO', 'Executed get_products tool');
      return getProductsHandler(args, extra);
    },
  );

  server.registerTool(
    "get_product_by_id",
    description: "Get a single product from the inventory database by its ID",
    inputSchema: JsonSchema.object(
      properties: {
        "id": JsonSchema.string(description: "The ID of the product to get"),
      },
      required: ["id"],
    ),
    callback: (args, extra) async {
      log('INFO', 'Executed get_product_by_id tool', {'args': args});
      return getProductByIdHandler(args, extra);
    },
  );

  server.registerTool(
    "search",
    description: "Search for products in the inventory database by name",
    inputSchema: JsonSchema.object(
      properties: {
        "query": JsonSchema.string(description: "The search query to filter products by name"),
      },
      required: ["query"],
    ),
    callback: (args, extra) async {
      log('INFO', 'Executed search tool', {'args': args});
      return searchHandler(args, extra);
    },
  );

  server.registerTool(
    "seed",
    description: "Seed the inventory database with products.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log('INFO', 'Executed seed tool');
      return seedHandler(args, extra);
    },
  );

  server.registerTool(
    "reset",
    description: "Clear all products from the inventory database.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log('INFO', 'Executed reset tool');
      return resetHandler(args, extra);
    },
  );

  server.registerTool(
    "get_root",
    description: "Get a greeting from the Cymbal Superstore Inventory API.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log('INFO', 'Executed get_root tool');
      return getRootHandler(args, extra);
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

  // Initialize Environment and Firestore
  final env = DotEnv(includePlatformEnvironment: true)..load();
  final projectId = env['GOOGLE_CLOUD_PROJECT'];
  if (projectId != null) {
    Firestore.initialize(projectId,useApplicationDefaultAuth: true);
    log('INFO', 'Firestore initialized', {'projectId': projectId});
  } else {
    log('WARNING', 'GOOGLE_CLOUD_PROJECT not found in environment');
  }

  if (transportType == 'stdio') {
    final server = createServer();
    final transport = StdioServerTransport();
    await server.connect(transport);
    log('INFO', 'Connected via stdio');
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
    log('INFO', 'Server started', {'transport': 'http', 'host': host, 'port': port, 'path': path});
  }
}

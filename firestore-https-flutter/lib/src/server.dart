import 'tools.dart';
import 'logger.dart';

McpServer createServer() {
  final server = McpServer(
    Implementation(name: "mcp-https-flutter", version: "1.0.0"),
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
        "query": JsonSchema.string(
          description: "The search query to filter products by name",
        ),
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

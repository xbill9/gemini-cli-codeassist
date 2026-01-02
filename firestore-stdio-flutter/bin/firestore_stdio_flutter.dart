import 'dart:io';
import 'package:mcp_dart/mcp_dart.dart';
import 'package:firestore_stdio_flutter/tools.dart';
import 'package:dotenv/dotenv.dart';

void main() async {
  final env = DotEnv(includePlatformEnvironment: true)..load();
  final projectId = env['FIRESTORE_PROJECT_ID'] ?? env['GOOGLE_CLOUD_PROJECT'] ?? 'your-project-id';

  // Set up logging
  void log(String message) {
    stderr.writeln(message);
  }

  try {
    await initFirestore(projectId);
    log("Firestore initialized for project: $projectId");
  } catch (e) {
    log("Failed to initialize Firestore: $e");
  }

  final server = McpServer(
    Implementation(
      name: "inventory-server",
      version: "1.0.0",
    ),
    options: McpServerOptions(
      capabilities: ServerCapabilities(
        tools: ServerCapabilitiesTools(listChanged: true),
      ),
    ),
  );

  server.registerTool(
    "get_products",
    description: "Get a list of all products from the inventory database",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log("Executed get_products tool");
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
      log("Executed get_product_by_id tool");
      return getProductByIdHandler(args, extra);
    },
  );

  server.registerTool(
    "seed",
    description: "Seed the inventory database with products.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log("Executed seed tool");
      return seedHandler(args, extra);
    },
  );

  server.registerTool(
    "reset",
    description: "Clears all products from the inventory database.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log("Executed reset tool");
      return resetHandler(args, extra);
    },
  );

  server.registerTool(
    "get_root",
    description: "Get a greeting from the Cymbal Superstore Inventory API.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log("Executed get_root tool");
      return getRootHandler(args, extra);
    },
  );

  server.registerTool(
    "check_db",
    description: "Checks if the inventory database is running.",
    inputSchema: JsonSchema.object(properties: {}),
    callback: (args, extra) async {
      log("Executed check_db tool");
      return checkDbHandler(args, extra);
    },
  );

  final transport = StdioServerTransport();
  await server.connect(transport);
}
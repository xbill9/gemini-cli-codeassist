import 'package:firestore_https_flutter/firestore_https_flutter.dart';
import 'package:firedart/firedart.dart';
import 'package:dotenv/dotenv.dart';

void main() async {
  // 1. Initialize configuration and Firestore
  final env = DotEnv(includePlatformEnvironment: true)..load();
  final projectId = env['GOOGLE_CLOUD_PROJECT'];

  if (projectId == null) {
    print('Error: GOOGLE_CLOUD_PROJECT environment variable not set.');
    print('Please set it in a .env file or your environment.');
    return;
  }

  // Initialize Firestore using application default credentials
  // This requires you to be logged in via `gcloud auth application-default login`
  // or running in a GCP environment.
  try {
    Firestore.initialize(projectId, useApplicationDefaultAuth: true);
    print('Firestore initialized with project: $projectId');
  } catch (e) {
    print("Failed to initialize firestore: $e");
    return;
  }

  try {
    // 2. Reset the database
    print('\n--- Resetting Database ---');
    final resetResult = await resetHandler({}, null);
    _printResult('reset', resetResult);

    // 3. Seed the database
    print('\n--- Seeding Database ---');
    final seedResult = await seedHandler({}, null);
    _printResult('seed', seedResult);

    // 4. Get all products
    print('\n--- Getting All Products ---');
    final productsResult = await getProductsHandler({}, null);
    // Truncate output for readability in console
    _printResult('get_products', productsResult, truncate: true);

    // 5. Find "Coffee"
    print('\n--- Searching for "Coffee" ---');
    final searchResult = await searchHandler({'query': 'Coffee'}, null);
    _printResult('search', searchResult);
  } catch (e) {
    print('An unexpected error occurred: $e');
  }
}

void _printResult(
  String toolName,
  CallToolResult result, {
  bool truncate = false,
}) {
  if (result.isError) {
    print('❌ Error executing $toolName.');
  } else {
    print('✅ $toolName executed successfully.');
  }

  if (result.content.isNotEmpty) {
    for (var item in result.content) {
      if (item is TextContent) {
        String text = item.text;
        if (truncate && text.length > 200) {
          print('Output: ${text.substring(0, 200)}... (truncated)');
        } else {
          print('Output: $text');
        }
      }
    }
  }
}

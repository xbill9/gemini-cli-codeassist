export 'package:mcp_dart/mcp_dart.dart';
import 'package:mcp_dart/mcp_dart.dart';
import 'package:firedart/firedart.dart';
import 'product.dart';
import 'logger.dart';
import 'dart:math';
import 'dart:convert';

/// Handles the 'greet' tool call.
Future<CallToolResult> greetHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling greet request', {'args': args});
  final name = args['name'] as String?;

  if (name != null) {
    return CallToolResult(content: [TextContent(text: "Hello, $name!")]);
  } else {
    log('WARNING', 'greet request missing name argument');
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Missing 'name' argument")],
    );
  }
}

/// Handles the 'get_products' tool call.
Future<CallToolResult> getProductsHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling get_products request');
  try {
    final docs = await Firestore.instance.collection('inventory').get();
    final products = docs
        .map((doc) => Product.fromFirestore(doc).toJson())
        .toList();
    log('INFO', 'Successfully retrieved ${products.length} products');
    return CallToolResult(content: [TextContent(text: jsonEncode(products))]);
  } catch (e) {
    log('ERROR', 'Error fetching products', {'error': e.toString()});
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error fetching products: $e")],
    );
  }
}

/// Handles the 'get_product_by_id' tool call.
Future<CallToolResult> getProductByIdHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling get_product_by_id request', {'args': args});
  final id = args['id'] as String?;
  if (id == null) {
    log('WARNING', 'get_product_by_id request missing id argument');
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Missing 'id' argument")],
    );
  }
  try {
    final doc = await Firestore.instance
        .collection('inventory')
        .document(id)
        .get();
    final product = Product.fromFirestore(doc);
    log('INFO', 'Successfully retrieved product', {'id': id});
    return CallToolResult(
      content: [TextContent(text: jsonEncode(product.toJson()))],
    );
  } catch (e) {
    log('ERROR', 'Error fetching product', {'id': id, 'error': e.toString()});
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error fetching product $id: $e")],
    );
  }
}

/// Handles the 'search' tool call.
Future<CallToolResult> searchHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling search request', {'args': args});
  final query = args['query'] as String?;
  if (query == null) {
    log('WARNING', 'search request missing query argument');
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Missing 'query' argument")],
    );
  }
  try {
    final docs = await Firestore.instance.collection('inventory').get();
    final results = docs
        .map((doc) => Product.fromFirestore(doc))
        .where((p) => p.name.toLowerCase().contains(query.toLowerCase()))
        .map((p) => p.toJson())
        .toList();
    log('INFO', 'Search found ${results.length} results', {'query': query});
    return CallToolResult(content: [TextContent(text: jsonEncode(results))]);
  } catch (e) {
    log('ERROR', 'Error searching products', {
      'query': query,
      'error': e.toString(),
    });
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error searching products: $e")],
    );
  }
}

/// Handles the 'seed' tool call.
Future<CallToolResult> seedHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling seed request');
  try {
    await _initFirestoreCollection();
    log('INFO', 'Database seeded successfully');
    return CallToolResult(
      content: [TextContent(text: "Database seeded successfully.")],
    );
  } catch (e) {
    log('ERROR', 'Error seeding database', {'error': e.toString()});
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error seeding database: $e")],
    );
  }
}

/// Handles the 'reset' tool call.
Future<CallToolResult> resetHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling reset request');
  try {
    await _cleanFirestoreCollection();
    log('INFO', 'Database reset successfully');
    return CallToolResult(
      content: [TextContent(text: "Database reset successfully.")],
    );
  } catch (e) {
    log('ERROR', 'Error resetting database', {'error': e.toString()});
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error resetting database: $e")],
    );
  }
}

/// Handles the 'get_root' tool call.
Future<CallToolResult> getRootHandler(
  Map<String, dynamic> args,
  dynamic extra,
) async {
  log('INFO', 'Handling get_root request');
  return CallToolResult(
    content: [
      TextContent(
        text: "🍎 Hello! This is the Cymbal Superstore Inventory API.",
      ),
    ],
  );
}

/// Cleans up the 'inventory' collection in Firestore.
Future<void> _cleanFirestoreCollection() async {
  final collectionRef = Firestore.instance.collection('inventory');
  final snapshot = await collectionRef.get();

  for (var doc in snapshot) {
    await collectionRef.document(doc.id).delete();
  }
}

/// Seeds the 'inventory' collection with sample products.
Future<void> _initFirestoreCollection() async {
  final oldProducts = [
    "Apples",
    "Bananas",
    "Milk",
    "Whole Wheat Bread",
    "Eggs",
    "Cheddar Cheese",
    "Whole Chicken",
    "Rice",
    "Black Beans",
    "Bottled Water",
    "Apple Juice",
    "Cola",
    "Coffee Beans",
    "Green Tea",
    "Watermelon",
    "Broccoli",
    "Jasmine Rice",
    "Yogurt",
    "Beef",
    "Shrimp",
    "Walnuts",
    "Sunflower Seeds",
    "Fresh Basil",
    "Cinnamon",
  ];

  final random = Random();

  for (final productName in oldProducts) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: random.nextInt(500) + 1,
      imgfile:
          "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(days: random.nextInt(365))),
      actualDateAdded: DateTime.now(),
    );
    await _addOrUpdateFirestore(product);
  }

  final recentProducts = [
    "Parmesan Crisps",
    "Pineapple Kombucha",
    "Maple Almond Butter",
    "Mint Chocolate Cookies",
    "White Chocolate Caramel Corn",
    "Acai Smoothie Packs",
    "Smores Cereal",
    "Peanut Butter and Jelly Cups",
  ];

  for (final productName in recentProducts) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: random.nextInt(100) + 1,
      imgfile:
          "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(days: random.nextInt(7))),
      actualDateAdded: DateTime.now(),
    );
    await _addOrUpdateFirestore(product);
  }

  final recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
  for (final productName in recentProductsOutOfStock) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: 0,
      imgfile:
          "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(days: random.nextInt(7))),
      actualDateAdded: DateTime.now(),
    );
    await _addOrUpdateFirestore(product);
  }
}

/// Adds or updates a product in Firestore based on its name.
Future<void> _addOrUpdateFirestore(Product product) async {
  final collection = Firestore.instance.collection("inventory");
  final query = await collection.where("name", isEqualTo: product.name).get();

  if (query.isEmpty) {
    await collection.add(product.toFirestore());
  } else {
    for (final doc in query) {
      await collection.document(doc.id).update(product.toFirestore());
    }
  }
}

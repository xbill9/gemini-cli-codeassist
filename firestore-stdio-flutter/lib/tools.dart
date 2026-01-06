import 'dart:convert';
import 'dart:math';
import 'package:firedart/firedart.dart';
import 'package:mcp_dart/mcp_dart.dart';
import 'product.dart';

bool dbRunning = false;

Future<void> initFirestore(String projectId) async {
  try {
    Firestore.initialize(projectId, useApplicationDefaultAuth: true);
    dbRunning = true;
  } catch (e) {
    dbRunning = false;
    rethrow;
  }
}

Future<CallToolResult> getProductsHandler(Map<String, dynamic> args, dynamic extra) async {
  if (!dbRunning) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Inventory database is not running.")],
    );
  }
  try {
    final products = await Firestore.instance.collection("inventory").get();
    final productsList = products.map((doc) => Product.fromFirestore(doc).toJson()).toList();
    return CallToolResult(
      content: [TextContent(text: jsonEncode(productsList))],
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error getting products: $e")],
    );
  }
}

Future<CallToolResult> getProductByIdHandler(Map<String, dynamic> args, dynamic extra) async {
  final id = args['id'] as String?;
  if (id == null) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Missing 'id' argument")],
    );
  }
  if (!dbRunning) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Inventory database is not running.")],
    );
  }
  try {
    final doc = await Firestore.instance.collection("inventory").document(id).get();
    final product = Product.fromFirestore(doc);
    return CallToolResult(
      content: [TextContent(text: jsonEncode(product.toJson()))],
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Product not found or error: $e")],
    );
  }
}

Future<CallToolResult> seedHandler(Map<String, dynamic> args, dynamic extra) async {
  if (!dbRunning) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Inventory database is not running.")],
    );
  }
  try {
    await initFirestoreCollection();
    return CallToolResult(
      content: [TextContent(text: "Database seeded successfully.")],
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error seeding database: $e")],
    );
  }
}

Future<CallToolResult> resetHandler(Map<String, dynamic> args, dynamic extra) async {
  if (!dbRunning) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Inventory database is not running.")],
    );
  }
  try {
    await cleanFirestoreCollection();
    return CallToolResult(
      content: [TextContent(text: "Database reset successfully.")],
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error resetting database: $e")],
    );
  }
}

Future<CallToolResult> getRootHandler(Map<String, dynamic> args, dynamic extra) async {
  return CallToolResult(
    content: [TextContent(text: "🍎 Hello! This is the Cymbal Superstore Inventory API.")],
  );
}

Future<CallToolResult> checkDbHandler(Map<String, dynamic> args, dynamic extra) async {
  return CallToolResult(
    content: [TextContent(text: "Database running: $dbRunning")],
  );
}

// Helper functions for seeding
Future<void> initFirestoreCollection() async {
  final random = Random();
  final oldProducts = [
    "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
    "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
    "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
    "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
    "Sunflower Seeds", "Fresh Basil", "Cinnamon",
  ];

  final futures = <Future>[];

  for (final productName in oldProducts) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: random.nextInt(500) + 1,
      imgfile: "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(milliseconds: (random.nextDouble() * 31536000000).toInt() + 7776000000)),
      actualdateadded: DateTime.now(),
    );
    futures.add(addOrUpdateFirestore(product));
  }

  final recentProducts = [
    "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
    "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
    "Smores Cereal", "Peanut Butter and Jelly Cups",
  ];

  for (final productName in recentProducts) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: random.nextInt(100) + 1,
      imgfile: "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(milliseconds: (random.nextDouble() * 518400000).toInt() + 1)),
      actualdateadded: DateTime.now(),
    );
    futures.add(addOrUpdateFirestore(product));
  }

  final recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
  for (final productName in recentProductsOutOfStock) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: 0,
      imgfile: "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(milliseconds: (random.nextDouble() * 518400000).toInt() + 1)),
      actualdateadded: DateTime.now(),
    );
    futures.add(addOrUpdateFirestore(product));
  }

  await Future.wait(futures);
}

Future<void> addOrUpdateFirestore(Product product) async {
  final collection = Firestore.instance.collection("inventory");
  final querySnapshot = await collection.where("name", isEqualTo: product.name).get();

  if (querySnapshot.isEmpty) {
    await collection.add(product.toFirestore());
  } else {
    await Future.wait(querySnapshot.map((doc) =>
        collection.document(doc.id).update(product.toFirestore())));
  }
}

Future<void> cleanFirestoreCollection() async {
  final collection = Firestore.instance.collection("inventory");
  final snapshot = await collection.get();
  await Future.wait(snapshot.map((doc) => collection.document(doc.id).delete()));
}
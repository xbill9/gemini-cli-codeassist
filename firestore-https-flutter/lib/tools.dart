library tools;

export 'package:mcp_dart/mcp_dart.dart';
import 'package:mcp_dart/mcp_dart.dart';
import 'package:firedart/firedart.dart';
import 'dart:math';
import 'dart:convert';

class Product {
  final String? id;
  final String name;
  final double price;
  final int quantity;
  final String imgfile;
  final DateTime timestamp;
  final DateTime actualdateadded;

  Product({
    this.id,
    required this.name,
    required this.price,
    required this.quantity,
    required this.imgfile,
    required this.timestamp,
    required this.actualdateadded,
  });

  Map<String, dynamic> toJson() => {
    'id': id,
    'name': name,
    'price': price,
    'quantity': quantity,
    'imgfile': imgfile,
    'timestamp': timestamp.toIso8601String(),
    'actualdateadded': actualdateadded.toIso8601String(),
  };

  factory Product.fromFirestore(Document doc) {
    final data = doc.map;
    return Product(
      id: doc.id,
      name: data['name'] ?? '',
      price: (data['price'] ?? 0).toDouble(),
      quantity: data['quantity'] ?? 0,
      imgfile: data['imgfile'] ?? '',
      timestamp: data['timestamp'] ?? DateTime.now(),
      actualdateadded: data['actualdateadded'] ?? DateTime.now(),
    );
  }

  Map<String, dynamic> toFirestore() => {
    'name': name,
    'price': price,
    'quantity': quantity,
    'imgfile': imgfile,
    'timestamp': timestamp,
    'actualdateadded': actualdateadded,
  };
}

Future<CallToolResult> greetHandler(Map<String, dynamic> args, dynamic extra) async {
  final name = args['name'] as String?;
  
  if (name != null) {
    return CallToolResult(
      content: [
        TextContent(
          text: "Hello, $name!"
        )
      ]
    );
  } else {
      return CallToolResult(
      isError: true,
      content: [
        TextContent(
          text: "Missing 'name' argument"
        )
      ]
      );
  }
}

Future<CallToolResult> getProductsHandler(Map<String, dynamic> args, dynamic extra) async {
  try {
    final docs = await Firestore.instance.collection('inventory').get();
    final products = docs.map((doc) => Product.fromFirestore(doc).toJson()).toList();
    return CallToolResult(
      content: [
        TextContent(
          text: jsonEncode(products)
        )
      ]
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error fetching products: $e")]
    );
  }
}

Future<CallToolResult> getProductByIdHandler(Map<String, dynamic> args, dynamic extra) async {
  final id = args['id'] as String?;
  if (id == null) {
    return CallToolResult(isError: true, content: [TextContent(text: "Missing 'id' argument")]);
  }
  try {
    final doc = await Firestore.instance.collection('inventory').document(id).get();
    final product = Product.fromFirestore(doc);
    return CallToolResult(
      content: [
        TextContent(
          text: jsonEncode(product.toJson())
        )
      ]
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error fetching product $id: $e")]
    );
  }
}

Future<CallToolResult> searchHandler(Map<String, dynamic> args, dynamic extra) async {
  final query = args['query'] as String?;
  if (query == null) {
    return CallToolResult(isError: true, content: [TextContent(text: "Missing 'query' argument")]);
  }
  try {
    final docs = await Firestore.instance.collection('inventory').get();
    final results = docs
        .map((doc) => Product.fromFirestore(doc))
        .where((p) => p.name.toLowerCase().contains(query.toLowerCase()))
        .map((p) => p.toJson())
        .toList();
    return CallToolResult(
      content: [
        TextContent(
          text: jsonEncode(results)
        )
      ]
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error searching products: $e")]
    );
  }
}

Future<CallToolResult> seedHandler(Map<String, dynamic> args, dynamic extra) async {
  try {
    await _initFirestoreCollection();
    return CallToolResult(
      content: [TextContent(text: "Database seeded successfully.")]
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error seeding database: $e")]
    );
  }
}

Future<CallToolResult> resetHandler(Map<String, dynamic> args, dynamic extra) async {
  try {
    await _cleanFirestoreCollection();
    return CallToolResult(
      content: [TextContent(text: "Database reset successfully.")]
    );
  } catch (e) {
    return CallToolResult(
      isError: true,
      content: [TextContent(text: "Error resetting database: $e")]
    );
  }
}

Future<CallToolResult> getRootHandler(Map<String, dynamic> args, dynamic extra) async {
  return CallToolResult(
    content: [TextContent(text: "🍎 Hello! This is the Cymbal Superstore Inventory API.")]
  );
}

Future<void> _cleanFirestoreCollection() async {
  final collectionRef = Firestore.instance.collection('inventory');
  final snapshot = await collectionRef.get();
  
  for (var doc in snapshot) {
    await collectionRef.document(doc.id).delete();
  }
}

Future<void> _initFirestoreCollection() async {
  final oldProducts = [
    "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
    "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
    "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
    "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
    "Sunflower Seeds", "Fresh Basil", "Cinnamon",
  ];

  final random = Random();

  for (final productName in oldProducts) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: random.nextInt(500) + 1,
      imgfile: "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(days: random.nextInt(365))),
      actualdateadded: DateTime.now(),
    );
    await _addOrUpdateFirestore(product);
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
      timestamp: DateTime.now().subtract(Duration(days: random.nextInt(7))),
      actualdateadded: DateTime.now(),
    );
    await _addOrUpdateFirestore(product);
  }

  final recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
  for (final productName in recentProductsOutOfStock) {
    final product = Product(
      name: productName,
      price: (random.nextInt(10) + 1).toDouble(),
      quantity: 0,
      imgfile: "product-images/${productName.replaceAll(' ', '').toLowerCase()}.png",
      timestamp: DateTime.now().subtract(Duration(days: random.nextInt(7))),
      actualdateadded: DateTime.now(),
    );
    await _addOrUpdateFirestore(product);
  }
}

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

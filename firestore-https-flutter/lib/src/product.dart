import 'package:firedart/firedart.dart';

/// Represents a product in the inventory.
class Product {
  /// The unique identifier of the product.
  final String? id;

  /// The name of the product.
  final String name;

  /// The price of the product.
  final double price;

  /// The quantity in stock.
  final int quantity;

  /// The image filename.
  final String imgfile;

  /// The timestamp of the product creation/update.
  final DateTime timestamp;

  /// The actual date the product was added.
  final DateTime actualDateAdded;

  /// Creates a new [Product].
  Product({
    this.id,
    required this.name,
    required this.price,
    required this.quantity,
    required this.imgfile,
    required this.timestamp,
    required this.actualDateAdded,
  });

  /// Converts the product to a JSON map.
  Map<String, dynamic> toJson() => {
        'id': id,
        'name': name,
        'price': price,
        'quantity': quantity,
        'imgfile': imgfile,
        'timestamp': timestamp.toIso8601String(),
        'actualdateadded': actualDateAdded.toIso8601String(),
      };

  /// Creates a [Product] from a Firestore [Document].
  factory Product.fromFirestore(Document doc) {
    final data = doc.map;
    return Product(
      id: doc.id,
      name: data['name'] ?? '',
      price: (data['price'] ?? 0).toDouble(),
      quantity: data['quantity'] ?? 0,
      imgfile: data['imgfile'] ?? '',
      timestamp: data['timestamp'] ?? DateTime.now(),
      actualDateAdded: data['actualdateadded'] ?? DateTime.now(),
    );
  }

  /// Converts the product to a Firestore-compatible map.
  Map<String, dynamic> toFirestore() => {
        'name': name,
        'price': price,
        'quantity': quantity,
        'imgfile': imgfile,
        'timestamp': timestamp,
        'actualdateadded': actualDateAdded,
      };
}

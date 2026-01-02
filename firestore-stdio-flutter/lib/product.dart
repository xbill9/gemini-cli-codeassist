import 'package:firedart/firedart.dart';

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

  factory Product.fromFirestore(Document doc) {
    final data = doc.map;
    return Product(
      id: doc.id,
      name: data['name'] as String,
      price: (data['price'] as num).toDouble(),
      quantity: data['quantity'] as int,
      imgfile: data['imgfile'] as String,
      timestamp: data['timestamp'] as DateTime,
      actualdateadded: data['actualdateadded'] as DateTime,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name,
      'price': price,
      'quantity': quantity,
      'imgfile': imgfile,
      'timestamp': timestamp.toIso8601String(),
      'actualdateadded': actualdateadded.toIso8601String(),
    };
  }

  Map<String, dynamic> toFirestore() {
    return {
      'name': name,
      'price': price,
      'quantity': quantity,
      'imgfile': imgfile,
      'timestamp': timestamp,
      'actualdateadded': actualdateadded,
    };
  }
}

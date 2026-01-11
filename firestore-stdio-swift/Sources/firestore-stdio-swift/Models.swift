import Foundation

struct Product: Codable, Sendable {
  var id: String?
  var name: String
  var price: Double
  var quantity: Int
  var imgfile: String
  var timestamp: Date
  var actualdateadded: Date
}

// Helper structures for Firestore REST API
struct FirestoreDocument: Codable, Sendable {
  var name: String?  // "projects/.../databases/.../documents/..."
  var fields: [String: FirestoreValue]?
  var createTime: String?
  var updateTime: String?
}

struct FirestoreValue: Codable, Sendable {
  var stringValue: String?
  /// Firestore returns integers as strings in JSON to avoid 64-bit precision loss
  var integerValue: String?
  var doubleValue: Double?
  var timestampValue: String?  // RFC3339 string
  var booleanValue: Bool?
  var mapValue: FirestoreMapValue?
  var arrayValue: FirestoreArrayValue?
  var nullValue: String?  // "NULL_VALUE"

  // Helper to get Any value (simplified)
  // In a full implementation, we'd have better extraction logic
}

struct FirestoreMapValue: Codable, Sendable {
  var fields: [String: FirestoreValue]?
}

struct FirestoreArrayValue: Codable, Sendable {
  var values: [FirestoreValue]?
}

extension Product {
  func toFirestoreFields() -> [String: FirestoreValue] {
    return [
      "name": FirestoreValue(stringValue: name),
      /// Firestore might expect doubleValue or integerValue.
      "price": FirestoreValue(doubleValue: price),
      "quantity": FirestoreValue(integerValue: String(quantity)),
      "imgfile": FirestoreValue(stringValue: imgfile),
      "timestamp": FirestoreValue(timestampValue: ISO8601DateFormatter().string(from: timestamp)),
      "actualdateadded": FirestoreValue(
        timestampValue: ISO8601DateFormatter().string(from: actualdateadded)),
    ]
  }

  static func from(document: FirestoreDocument) -> Product? {
    guard let fields = document.fields,
      let namePath = document.name
    else { return nil }

    let id = namePath.components(separatedBy: "/").last

    guard let name = fields["name"]?.stringValue,
      let priceVal = fields["price"],
      let quantityVal = fields["quantity"],
      let imgfile = fields["imgfile"]?.stringValue,
      let timestampStr = fields["timestamp"]?.timestampValue,
      let actualdateaddedStr = fields["actualdateadded"]?.timestampValue,
      let timestamp = ISO8601DateFormatter.firestoreDate(from: timestampStr),
      let actualdateadded = ISO8601DateFormatter.firestoreDate(from: actualdateaddedStr)
    else { return nil }

    // Handle price (could be integer or double in Firestore)
    let price: Double
    if let d = priceVal.doubleValue {
      price = d
    } else if let i = priceVal.integerValue, let d = Double(i) {
      price = d
    } else {
      return nil
    }

    // Handle quantity
    let quantity: Int
    if let i = quantityVal.integerValue, let q = Int(i) { quantity = q } else { return nil }

    return Product(
      id: id,
      name: name,
      price: price,
      quantity: quantity,
      imgfile: imgfile,
      timestamp: timestamp,
      actualdateadded: actualdateadded
    )
  }
}

extension ISO8601DateFormatter {
  static var firestoreDate: ISO8601DateFormatter {
    let formatter = ISO8601DateFormatter()
    formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
    return formatter
  }

  static func firestoreDate(from string: String) -> Date? {
    // Handle cases without fractional seconds just in case
    if let date = ISO8601DateFormatter.firestoreDate.date(from: string) {
      return date
    }
    let fallback = ISO8601DateFormatter()
    fallback.formatOptions = [.withInternetDateTime]
    return fallback.date(from: string)
  }
}

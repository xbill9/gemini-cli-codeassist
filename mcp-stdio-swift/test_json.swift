import Foundation

struct Test: Encodable {
    let message: String
}

let encoder = JSONEncoder()
encoder.outputFormatting = [.sortedKeys, .withoutEscapingSlashes]

let test = Test(message: "hello/world")
do {
    let data = try encoder.encode(test)
    print(String(data: data, encoding: .utf8)!)
} catch {
    print("Error: \(error)")
}

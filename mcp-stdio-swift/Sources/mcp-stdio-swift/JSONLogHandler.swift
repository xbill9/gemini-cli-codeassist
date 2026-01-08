import Foundation
import Logging

public struct JSONLogHandler: LogHandler {
  public var logLevel: Logger.Level = .info
  public var metadata: Logger.Metadata = [:]

  private let label: String

  public init(label: String) {
    self.label = label
  }

  public subscript(metadataKey key: String) -> Logger.Metadata.Value? {
    get { metadata[key] }
    set { metadata[key] = newValue }
  }

  public func log(
    level: Logger.Level,
    message: Logger.Message,
    metadata: Logger.Metadata?,
    source: String,
    file: String,
    function: String,
    line: UInt
  ) {
    let effectiveMetadata = self.metadata.merging(metadata ?? [:]) { _, new in new }

    var jsonDictionary: [String: Any] = [
      "timestamp": ISO8601DateFormatter().string(from: Date()),
      "level": level.rawValue,
      "label": self.label,
      "message": message.description,
      "source": source,
    ]

    // Add metadata if present
    if !effectiveMetadata.isEmpty {
      jsonDictionary["metadata"] = effectiveMetadata.mapValues { $0.description }
    }

    // Serialize to JSON
    do {
      let jsonData = try JSONSerialization.data(withJSONObject: jsonDictionary, options: [])
      if let jsonString = String(data: jsonData, encoding: .utf8),
        let data = "\(jsonString)\n".data(using: .utf8)
      {
        try FileHandle.standardError.write(contentsOf: data)
      }
    } catch {
      // Fallback in case of JSON failure (should be rare)
      let fallbackString =
        "{\"error\": \"Failed to serialize log message\", \"original_message\": \"\(message)\"}\n"
      if let data = fallbackString.data(using: .utf8) {
        try? FileHandle.standardError.write(contentsOf: data)
      }
    }
  }
}

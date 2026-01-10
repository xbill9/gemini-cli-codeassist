import Foundation
import Logging

struct JSONLogHandler: LogHandler {
    let label: String
    var logLevel: Logger.Level = .info
    var metadata: Logger.Metadata = [:]

    init(label: String) {
        self.label = label
    }

    subscript(metadataKey key: String) -> Logger.Metadata.Value? {
        get { metadata[key] }
        set { metadata[key] = newValue }
    }

    func log(level: Logger.Level,
             message: Logger.Message,
             metadata: Logger.Metadata?,
             source: String,
             file: String,
             function: String,
             line: UInt) {
        
        let mergedMetadata = self.metadata.merging(metadata ?? [:]) { _, new in new }
        
        var json: [String: Any] = [
            "timestamp": ISO8601DateFormatter().string(from: Date()),
            "level": level.rawValue,
            "message": message.description,
            "label": label,
        ]
        
        if !mergedMetadata.isEmpty {
            json["metadata"] = mapMetadata(mergedMetadata)
        }
        
        // Serialize to JSON
        do {
            let data = try JSONSerialization.data(withJSONObject: json, options: [])
            if let jsonString = String(data: data, encoding: .utf8),
               let outputData = (jsonString + "\n").data(using: .utf8) {
                // Write to stderr
                try? FileHandle.standardError.write(contentsOf: outputData)
            }
        } catch {
            if let errorData = "{\"level\":\"error\",\"message\":\"Failed to serialize log message to JSON\"}\n".data(using: .utf8) {
                try? FileHandle.standardError.write(contentsOf: errorData)
            }
        }
    }

    private func mapMetadata(_ metadata: Logger.Metadata) -> [String: Any] {
        var output: [String: Any] = [: ]
        for (key, value) in metadata {
            output[key] = mapMetadataValue(value)
        }
        return output
    }

    private func mapMetadataValue(_ value: Logger.Metadata.Value) -> Any {
        switch value {
        case .string(let string):
            return string
        case .stringConvertible(let convertible):
            return convertible.description
        case .dictionary(let dict):
            return mapMetadata(dict)
        case .array(let array):
            return array.map { mapMetadataValue($0) }
        }
    }
}

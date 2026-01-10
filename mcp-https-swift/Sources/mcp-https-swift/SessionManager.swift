import Foundation
import Logging
import MCP

actor SessionManager {
  private var sessions: [String: SSEServerTransport] = [:]

  func createSession(logger: Logger) -> SSEServerTransport {
    let id = UUID().uuidString
    let transport = SSEServerTransport(sessionId: id, logger: logger)
    sessions[id] = transport
    return transport
  }

  func getSession(_ id: String) -> SSEServerTransport? {
    return sessions[id]
  }

  func removeSession(_ id: String) {
    sessions.removeValue(forKey: id)
  }

  func disconnectAll() async {
    for transport in sessions.values {
      await transport.disconnect()
    }
    sessions.removeAll()
  }
}

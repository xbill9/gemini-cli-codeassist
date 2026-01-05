import Foundation
import Logging
import MCP
import ServiceLifecycle

// Define MCPService to bridge between MCP and ServiceLifecycle
struct MCPService: Service {
  let server: Server
  let transport: Transport
  let logger: Logger

  func run() async throws {
    // Start the server
    try await server.start(transport: transport)

    // Wait for the server to complete or for the service to be shut down
    await withGracefulShutdownHandler {
      await server.waitUntilCompleted()
    } onGracefulShutdown: {
      Task {
        await server.stop()
      }
    }
  }
}

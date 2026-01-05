import ServiceLifecycle
import MCP
import Foundation
import Logging

// Define MCPService to bridge between MCP and ServiceLifecycle
struct MCPService: Service {
    let server: Server
    let transport: Transport
    let logger: Logger

    init(server: Server, transport: Transport, logger: Logger) {
        self.server = server
        self.transport = transport
        self.logger = logger
    }

    func run() async throws {
        // Start the server
        try await server.start(transport: transport)
        
        // Wait for the server to complete or for the service to be shut down
        try await withGracefulShutdownHandler {
            await server.waitUntilCompleted()
        } onGracefulShutdown: {
            Task {
                await server.stop()
            }
        }
    }
}

import Foundation
import Logging
import MCP
import ServiceLifecycle

// Set up logging to stderr to avoid interfering with stdout transport
LoggingSystem.bootstrap { label in
  StreamLogHandler.standardError(label: label)
}

var logger = Logger(label: "hello-world-server")
logger.logLevel = .trace

// Create the MCP server
let server = Server(
  name: "hello-world-server",
  version: "1.0.0",
  capabilities: .init(
    tools: .init(listChanged: true)
  )
)

let handlers = Handlers(logger: logger)

// Register ListTools handler
await server.withMethodHandler(ListTools.self, handler: handlers.listTools)

// Register CallTool handler
await server.withMethodHandler(CallTool.self, handler: handlers.callTool)

// Create MCP service and other services
// Explicitly use stdio transport
let transport = StdioTransport(logger: logger)
let mcpService = MCPService(server: server, transport: transport, logger: logger)

let serviceGroup = ServiceGroup(
  services: [mcpService],
  gracefulShutdownSignals: [.sigterm, .sigint],
  logger: logger
)

// Run the service group
try await serviceGroup.run()

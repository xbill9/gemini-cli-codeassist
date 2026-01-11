import AsyncHTTPClient
import Foundation
import Logging
import MCP
import NIO
import ServiceLifecycle

// Set up logging to stderr to avoid interfering with stdout transport
LoggingSystem.bootstrap { label in
  JSONLogHandler(label: label)
}

var logger = Logger(label: "firestore-stdio-server")
logger.logLevel = .info

// Create the MCP server
let server = Server(
  name: "inventory-server",
  version: "1.0.0",
  capabilities: .init(
    tools: .init(listChanged: true)
  )
)

// Setup Firestore
let httpClient = HTTPClient(eventLoopGroupProvider: .singleton)
var firestoreClient: FirestoreClient? = nil

if let credentialsPath = ProcessInfo.processInfo.environment["GOOGLE_APPLICATION_CREDENTIALS"] {
  do {
    let fileUrl = URL(fileURLWithPath: credentialsPath)
    let data = try Data(contentsOf: fileUrl)
    let serviceAccount = try JSONDecoder().decode(ServiceAccount.self, from: data)
    let authProvider = GoogleTokenProvider(
      serviceAccount: serviceAccount, httpClient: httpClient, logger: logger)
    firestoreClient = FirestoreClient(auth: authProvider, httpClient: httpClient, logger: logger)
    logger.info("Firestore client initialized with credentials at \(credentialsPath)")
  } catch {
    logger.error("Failed to initialize Firestore credentials: \(error)")
  }
} else {
  logger.warning("GOOGLE_APPLICATION_CREDENTIALS not set. Firestore features will be disabled.")
}

let handlers = Handlers(logger: logger, firestore: firestoreClient)

// Register ListTools handler
await server.withMethodHandler(ListTools.self, handler: handlers.listTools)

// Register CallTool handler
await server.withMethodHandler(CallTool.self, handler: handlers.callTool)

// Create MCP service and other services
let transport = StdioTransport(logger: logger)
let mcpService = MCPService(server: server, transport: transport, logger: logger)

// Service to manage HTTP Client lifecycle
struct HTTPClientService: Service {
  let client: HTTPClient
  func run() async throws {
    // Keep running until cancelled
    try await withTaskCancellationHandler {
      try await Task.sleep(nanoseconds: UInt64.max)
    } onCancel: {
      try? client.syncShutdown()
    }
  }
}

// We need to shut down HTTP client properly.
// ServiceLifecycle's ServiceGroup handles this if we wrap it.
// However, HTTPClient.shutdown() is async or sync.
// Let's just defer shutdown in main or use a simple service wrapper.

let serviceGroup = ServiceGroup(
  services: [mcpService],
  gracefulShutdownSignals: [.sigterm, .sigint],
  logger: logger
)

// Run the service group
do {
  try await serviceGroup.run()
} catch {
  logger.error("Service group error: \(error)")
}

// Cleanup
try? await httpClient.shutdown().get()

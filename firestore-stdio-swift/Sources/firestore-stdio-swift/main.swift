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

let logger = {
  var logger = Logger(label: "firestore-stdio-server")
  logger.logLevel = .info
  return logger
}()

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

func initializeFirestore(logger: Logger) async -> FirestoreClient? {
  // Check for GOOGLE_APPLICATION_CREDENTIALS or default location
  var credentialsPath = ProcessInfo.processInfo.environment["GOOGLE_APPLICATION_CREDENTIALS"]

  if credentialsPath == nil {
    let defaultPath = FileManager.default.homeDirectoryForCurrentUser
      .appendingPathComponent(".config/gcloud/application_default_credentials.json").path
    if FileManager.default.fileExists(atPath: defaultPath) {
      credentialsPath = defaultPath
      logger.info(
        "GOOGLE_APPLICATION_CREDENTIALS not set. Using default credentials at \(defaultPath)")
    }
  }

  if let credentialsPath {
    do {
      let fileUrl = URL(fileURLWithPath: credentialsPath)
      let data = try Data(contentsOf: fileUrl)
      let serviceAccount = try JSONDecoder().decode(ServiceAccount.self, from: data)
      let authProvider = ServiceAccountTokenProvider(
        serviceAccount: serviceAccount, httpClient: httpClient, logger: logger)
      let client = FirestoreClient(auth: authProvider, httpClient: httpClient, logger: logger)
      logger.info("Firestore client initialized with credentials at \(credentialsPath)")
      return client
    } catch {
      logger.error(
        "Failed to initialize Firestore credentials: \(error). Falling back to Metadata Server."
      )
    }
  }

  // Try Metadata Server (Cloud Run/GCE)
  let metadataProvider = MetadataServerTokenProvider(httpClient: httpClient, logger: logger)
  do {
    // Test connection to metadata server
    _ = try await metadataProvider.getProjectId()
    logger.info("Firestore client initialized with Google Metadata Server")
    return FirestoreClient(auth: metadataProvider, httpClient: httpClient, logger: logger)
  } catch {
    logger.warning(
      "GOOGLE_APPLICATION_CREDENTIALS not set and Metadata Server not available. Firestore features will be disabled."
    )
    return nil
  }
}

let firestoreClient = await initializeFirestore(logger: logger)
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

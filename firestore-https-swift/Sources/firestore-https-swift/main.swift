import Foundation
import HTTPTypes
import Hummingbird
import Logging
import MCP
import NIOCore
import ServiceLifecycle
import AsyncHTTPClient
import NIO

// Logging setup
LoggingSystem.bootstrap(JSONLogHandler.init)
let logger = {
  var logger = Logger(label: "firestore-https-swift")
  logger.logLevel = .trace
  return logger
}()

// Setup Firestore
let httpClient = HTTPClient(eventLoopGroupProvider: .singleton)

func initializeFirestore() async -> FirestoreClient {
    if let credentialsPath = ProcessInfo.processInfo.environment["GOOGLE_APPLICATION_CREDENTIALS"] {
        do {
            let fileUrl = URL(fileURLWithPath: credentialsPath)
            let data = try Data(contentsOf: fileUrl)
            let serviceAccount = try JSONDecoder().decode(ServiceAccount.self, from: data)
            let authProvider = ServiceAccountTokenProvider(serviceAccount: serviceAccount, httpClient: httpClient, logger: logger)
            let client = FirestoreClient(auth: authProvider, httpClient: httpClient, logger: logger)
            logger.info("Firestore client initialized with credentials at \(credentialsPath)")
            return client
        } catch {
            logger.error("Failed to initialize Firestore credentials: \(error). Falling back to in-memory storage.")
            return FirestoreClient(auth: nil, httpClient: httpClient, logger: logger)
        }
    } else {
        // Try Metadata Server (Cloud Run/GCE)
        let metadataProvider = MetadataServerTokenProvider(httpClient: httpClient, logger: logger)
        do {
            // Test connection to metadata server
            let _ = try await metadataProvider.getProjectId()
            logger.info("Firestore client initialized with Google Metadata Server")
            return FirestoreClient(auth: metadataProvider, httpClient: httpClient, logger: logger)
        } catch {
            logger.warning("GOOGLE_APPLICATION_CREDENTIALS not set and Metadata Server not available. Firestore will run in in-memory mode.")
            return FirestoreClient(auth: nil, httpClient: httpClient, logger: logger)
        }
    }
}

let firestoreClient = await initializeFirestore()

// Session Manager
let sessionManager = SessionManager()

// Router
let router = Router()

// Capture firestoreClient for closures
let sharedFirestoreClient = firestoreClient

// GET /mcp - Start SSE Session
router.get("/mcp") { request, context -> Response in
  let transport = await sessionManager.createSession(logger: logger)
  await transport.sendEndpointEvent("/mcp?sessionId=\(transport.sessionId)")
  let sessionId = transport.sessionId

  let server = Server(
    name: "firestore-https-swift",
    version: "1.0.0",
    capabilities: .init(
      tools: .init(listChanged: true)
    )
  )

  let handlers = Handlers(logger: logger, firestore: sharedFirestoreClient)
  await server.withMethodHandler(ListTools.self, handler: handlers.listTools)
  await server.withMethodHandler(CallTool.self, handler: handlers.callTool)

  // Start server in background
  Task {
    do {
      try await server.start(transport: transport)
      await server.waitUntilCompleted()
      logger.info("Server session ended: \(sessionId)")
      await sessionManager.removeSession(sessionId)
    } catch {
      logger.error("Server session error: \(error)")
      await sessionManager.removeSession(sessionId)
    }
  }

  // Create SSE Stream
  let stream = transport.sseStream.map { event -> ByteBuffer in
    let string = "event: \(event.event)\ndata: \(event.data)\n\n"
    return ByteBuffer(string: string)
  }

  var headers = HTTPFields()
  headers[HTTPField.Name.contentType] = "text/event-stream"
  headers[HTTPField.Name.cacheControl] = "no-cache"
  headers[HTTPField.Name.connection] = "keep-alive"
  headers[HTTPField.Name("Mcp-Session-Id")!] = sessionId

  return Response(status: .ok, headers: headers, body: .init(asyncSequence: stream))
}

// DELETE /mcp - Terminate Session
router.delete("/mcp") { request, context -> HTTPResponse.Status in
  guard let sessionId = request.headers[HTTPField.Name("Mcp-Session-Id")!] else {
    if let query = request.uri.query {
      let params = query.split(separator: "&")
      for param in params {
        let pair = param.split(separator: "=")
        if pair.count == 2, pair[0] == "sessionId" {
          let sid = String(pair[1])
          await sessionManager.removeSession(sid)
          return .noContent
        }
      }
    }
    throw HTTPError(.badRequest, message: "Missing Mcp-Session-Id header")
  }

  await sessionManager.removeSession(sessionId)
  return .noContent
}


// POST /mcp - Receive Message
router.post("/mcp") { request, context -> HTTPResponse.Status in
  var sessionId = request.headers[HTTPField.Name("Mcp-Session-Id")!]

  if sessionId == nil {
    if let query = request.uri.query {
      let params = query.split(separator: "&")
      for param in params {
        let pair = param.split(separator: "=")
        if pair.count == 2, pair[0] == "sessionId" {
          sessionId = String(pair[1])
          break
        }
      }
    }
  }

  guard let finalSessionId = sessionId else {
    throw HTTPError(.badRequest, message: "Missing Mcp-Session-Id header")
  }

  guard let transport = await sessionManager.getSession(finalSessionId) else {
    throw HTTPError(.notFound, message: "Session not found")
  }

  let buffer = try await request.body.collect(upTo: 1024 * 1024)
  let data = Data(buffer.readableBytesView)

  await transport.handlePostData(data)

  return .accepted
}

// REST Handlers
router.get("/status") { request, context -> Response in
  let body = "{\"message\": \"Cymbal Superstore Inventory API is running.\", \"db\": true}"
  var headers = HTTPFields()
  headers[HTTPField.Name.contentType] = "application/json"
  return Response(status: .ok, headers: headers, body: .init(byteBuffer: ByteBuffer(string: body)))
}

router.get("/health") { request, context -> Response in
  let hostname = ProcessInfo.processInfo.hostName
  let body = "{\"message\": \"Healthy\", \"db\": true, \"hostname\": \"\(hostname)\"}"
  var headers = HTTPFields()
  headers[HTTPField.Name.contentType] = "application/json"
  return Response(status: .ok, headers: headers, body: .init(byteBuffer: ByteBuffer(string: body)))
}

// Application
let port = Int(ProcessInfo.processInfo.environment["PORT"] ?? "8080") ?? 8080
let app = Application(
  router: router,
  configuration: .init(address: .hostname("0.0.0.0", port: port)),
  logger: logger
)

struct SessionCleanupService: Service {
  let sessionManager: SessionManager
  let logger: Logger

  func run() async throws {
    let (stream, continuation) = AsyncStream.makeStream(of: Void.self)
    
    await withGracefulShutdownHandler {
      logger.debug("SessionCleanupService: Waiting for shutdown")
      for await _ in stream {}
    } onGracefulShutdown: {
      logger.debug("SessionCleanupService: Graceful shutdown triggered")
      continuation.finish()
    }

    logger.debug("SessionCleanupService: Disconnecting all sessions")
    await sessionManager.disconnectAll()
    logger.debug("SessionCleanupService: Finished")
  }
}

// Service Group
let serviceGroup = ServiceGroup(
  configuration: .init(
    services: [
      app,
      SessionCleanupService(sessionManager: sessionManager, logger: logger)
    ],
    gracefulShutdownSignals: [.sigterm, .sigint],
    logger: logger
  )
)

try await serviceGroup.run()

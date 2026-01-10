import Foundation
import HTTPTypes
import Hummingbird
import Logging
import MCP
import NIOCore
import ServiceLifecycle

// Logging setup
let logger = {
  var logger = Logger(label: "mcp-http-server")
  logger.logLevel = .trace
  return logger
}()

// Session Manager
let sessionManager = SessionManager()

// Router
let router = Router()

// GET /mcp - Start SSE Session
router.get("/mcp") { request, context -> Response in
  let transport = await sessionManager.createSession(logger: logger)
  await transport.sendEndpointEvent("/mcp?sessionId=\(transport.sessionId)")
  let sessionId = transport.sessionId

  let server = Server(
    name: "mcp-http-server",
    version: "1.0.0",
    capabilities: .init(
      tools: .init(listChanged: true)
    )
  )

  let handlers = Handlers(logger: logger)
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

// Application
let app = Application(
  router: router,
  configuration: .init(address: .hostname("0.0.0.0", port: 8080)),
  logger: logger
)

// Service Group
let serviceGroup = ServiceGroup(
  services: [app],
  gracefulShutdownSignals: [.sigterm, .sigint],
  logger: logger
)

try await serviceGroup.run()

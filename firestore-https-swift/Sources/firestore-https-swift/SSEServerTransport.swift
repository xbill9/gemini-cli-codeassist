import Foundation
import Hummingbird
import Logging
import MCP

struct ServerSentEvent: Sendable {
  let data: String
  let event: String
}

actor SSEServerTransport: Transport {
  let sessionId: String
  let logger: Logger

  // Inbound data (from POST requests)
  private let receiveStream: AsyncThrowingStream<Data, Error>
  private let receiveContinuation: AsyncThrowingStream<Data, Error>.Continuation

  // Outbound events (to SSE stream)
  let sseStream: AsyncStream<ServerSentEvent>
  private let sseContinuation: AsyncStream<ServerSentEvent>.Continuation

  init(sessionId: String, logger: Logger) {
    self.sessionId = sessionId
    self.logger = logger

    var receiveContinuation: AsyncThrowingStream<Data, Error>.Continuation!
    self.receiveStream = AsyncThrowingStream { continuation in
      receiveContinuation = continuation
    }
    self.receiveContinuation = receiveContinuation

    var sseContinuation: AsyncStream<ServerSentEvent>.Continuation!
    self.sseStream = AsyncStream { continuation in
      sseContinuation = continuation
    }
    self.sseContinuation = sseContinuation
  }

  func connect() async throws {
    // Connected
  }

  func disconnect() async {
    receiveContinuation.finish()
    sseContinuation.finish()
  }

  func send(_ data: Data) async throws {
    if let jsonString = String(data: data, encoding: .utf8) {
      sseContinuation.yield(ServerSentEvent(data: jsonString, event: "message"))
    }
  }

  func sendEndpointEvent(_ endpoint: String) {
    sseContinuation.yield(ServerSentEvent(data: endpoint, event: "endpoint"))
  }

  func receive() -> AsyncThrowingStream<Data, Error> {
    return receiveStream
  }

  func handlePostData(_ data: Data) {
    receiveContinuation.yield(data)
  }
}

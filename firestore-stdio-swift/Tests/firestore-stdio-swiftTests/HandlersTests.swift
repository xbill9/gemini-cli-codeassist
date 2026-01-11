import Logging
import MCP
import XCTest

@testable import firestore_stdio_swift

final class HandlersTests: XCTestCase {
  var handlers: Handlers!
  var logger: Logger!

  override func setUp() {
    super.setUp()
    logger = Logger(label: "test")
    // Pass nil for firestore client for testing basic handlers
    handlers = Handlers(logger: logger, firestore: nil)
  }

  func testListTools() async throws {
    let params = ListTools.Parameters()
    let result = try await handlers.listTools(params)

    XCTAssertEqual(result.tools.count, 6)
    XCTAssertTrue(result.tools.contains { $0.name == "get_root" })
    XCTAssertTrue(result.tools.contains { $0.name == "check_db" })
  }

  func testCallToolGetRoot() async throws {
    let params = CallTool.Parameters(name: "get_root", arguments: [:])
    let result = try await handlers.callTool(params)

    XCTAssertFalse(result.isError ?? false)
    XCTAssertEqual(result.content.count, 1)
    if case .text(let text) = result.content[0] {
      XCTAssertTrue(text.contains("Cymbal Superstore"))
    } else {
      XCTFail("Expected text content")
    }
  }

  func testCheckDbNotRunning() async throws {
    let params = CallTool.Parameters(name: "check_db", arguments: [:])
    let result = try await handlers.callTool(params)

    XCTAssertFalse(result.isError ?? false)
    if case .text(let text) = result.content[0] {
      XCTAssertEqual(text, "Database running: false")
    } else {
      XCTFail("Expected text content")
    }
  }
}

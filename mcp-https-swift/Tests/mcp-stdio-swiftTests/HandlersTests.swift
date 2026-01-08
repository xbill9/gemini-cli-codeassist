import Logging
import MCP
import XCTest

@testable import mcp_stdio_swift

final class HandlersTests: XCTestCase {
  var handlers: Handlers!
  var logger: Logger!

  override func setUp() {
    super.setUp()
    logger = Logger(label: "test")
    handlers = Handlers(logger: logger)
  }

  func testListTools() async throws {
    let params = ListTools.Parameters()
    let result = try await handlers.listTools(params)

    XCTAssertEqual(result.tools.count, 1)
    XCTAssertEqual(result.tools[0].name, "greet")
  }

  func testCallToolGreet() async throws {
    let params = CallTool.Parameters(name: "greet", arguments: ["param": .string("World")])
    let result = try await handlers.callTool(params)

    XCTAssertFalse(result.isError ?? false)
    XCTAssertEqual(result.content.count, 1)
    if case .text(let text) = result.content[0] {
      XCTAssertEqual(text, "World")
    } else {
      XCTFail("Expected text content")
    }
  }

  func testCallToolMissingParam() async throws {
    let params = CallTool.Parameters(name: "greet", arguments: [:])
    let result = try await handlers.callTool(params)

    XCTAssertTrue(result.isError ?? false)
    if case .text(let text) = result.content[0] {
      XCTAssertTrue(text.contains("Missing required parameter"))
    } else {
      XCTFail("Expected text content")
    }
  }
}

import Logging
import MCP
import XCTest

@testable import firestore_https_swift

final class HandlersTests: XCTestCase {
  var handlers: Handlers!
  var logger: Logger!

  override func setUp() {
    super.setUp()
    logger = Logger(label: "test")
    handlers = Handlers(logger: logger, firestore: nil)
  }

  func testListTools() async throws {
    let params = ListTools.Parameters()
    let result = try await handlers.listTools(params)

    XCTAssertEqual(result.tools.count, 7)
    let toolNames = result.tools.map { $0.name }
    XCTAssertTrue(toolNames.contains("greet"))
    XCTAssertTrue(toolNames.contains("get_products"))
    XCTAssertTrue(toolNames.contains("get_product_by_id"))
    XCTAssertTrue(toolNames.contains("search"))
    XCTAssertTrue(toolNames.contains("seed"))
    XCTAssertTrue(toolNames.contains("reset"))
    XCTAssertTrue(toolNames.contains("get_root"))
  }

  func testCallToolGreet() async throws {
    let params = CallTool.Parameters(name: "greet", arguments: ["name": .string("World")])
    let result = try await handlers.callTool(params)

    XCTAssertFalse(result.isError ?? false)
    XCTAssertEqual(result.content.count, 1)
    if case .text(let text) = result.content[0] {
      XCTAssertEqual(text, "Hello, World!")
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

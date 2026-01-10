import Logging
import MCP
import XCTest
import AsyncHTTPClient

@testable import firestore_https_swift

final class HandlersTests: XCTestCase {
  var handlers: Handlers!
  var logger: Logger!
  var httpClient: HTTPClient!

  override func setUp() {
    super.setUp()
    logger = Logger(label: "test")
    httpClient = HTTPClient(eventLoopGroupProvider: .singleton)
    let firestore = FirestoreClient(auth: nil, httpClient: httpClient, logger: logger)
    handlers = Handlers(logger: logger, firestore: firestore)
  }

  override func tearDown() {
    try? httpClient.syncShutdown()
    super.tearDown()
  }

  func testListTools() async throws {
    let params = ListTools.Parameters()
    let result = try await handlers.listTools(params)

    XCTAssertEqual(result.tools.count, 8)
    let toolNames = result.tools.map { $0.name }
    XCTAssertTrue(toolNames.contains("greet"))
    XCTAssertTrue(toolNames.contains("get_products"))
    XCTAssertTrue(toolNames.contains("get_product_by_id"))
    XCTAssertTrue(toolNames.contains("search"))
    XCTAssertTrue(toolNames.contains("seed"))
    XCTAssertTrue(toolNames.contains("reset"))
    XCTAssertTrue(toolNames.contains("get_root"))
    XCTAssertTrue(toolNames.contains("check_db"))
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

  func testCallToolSeedAndGetProducts() async throws {
    // 1. Seed
    let seedParams = CallTool.Parameters(name: "seed", arguments: [:])
    let seedResult = try await handlers.callTool(seedParams)
    XCTAssertFalse(seedResult.isError ?? false)

    // 2. Get Products
    let getParams = CallTool.Parameters(name: "get_products", arguments: [:])
    let getResult = try await handlers.callTool(getParams)
    XCTAssertFalse(getResult.isError ?? false)
    
    if case .text(let json) = getResult.content[0] {
      let products = try JSONDecoder().decode([Product].self, from: json.data(using: .utf8)!)
      XCTAssertGreaterThan(products.count, 0)
      XCTAssertTrue(products.contains(where: { $0.name == "Apples" }))
    } else {
      XCTFail("Expected text content")
    }
  }
}

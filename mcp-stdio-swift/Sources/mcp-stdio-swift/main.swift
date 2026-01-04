import Foundation
import MCP
import ServiceLifecycle
import Logging

// Set up logging to stderr to avoid interfering with stdout transport
LoggingSystem.bootstrap { label in
    StreamLogHandler.standardError(label: label)
}

let logger = Logger(label: "hello-world-server")

// Create the MCP server
let server = Server(
    name: "hello-world-server",
    version: "1.0.0",
    capabilities: .init(
        tools: .init(listChanged: true)
    )
)

// Register ListTools handler
await server.withMethodHandler(ListTools.self) { _ in
    let schemaJSON = """
    {
      "type": "object",
      "properties": {
        "param": {
          "type": "string",
          "description": "The name or parameter to greet"
        }
      },
      "required": ["param"]
    }
    """
    // Value is the JSON type in MCP SDK
    let schema = try! JSONDecoder().decode(Value.self, from: schemaJSON.data(using: .utf8)!)
    
    let tools = [
        Tool(
            name: "greet",
            description: "Get a greeting from a local stdio server.",
            inputSchema: schema
        )
    ]
    return .init(tools: tools)
}

// Register CallTool handler
await server.withMethodHandler(CallTool.self) { params in
    switch params.name {
    case "greet":
        logger.debug("Executed greet tool")
        
        guard let param = params.arguments?["param"]?.stringValue else {
             return .init(content: [.text("Missing required parameter: param")], isError: true)
        }
        
        // Return the param as per the original Python implementation
        return .init(
            content: [.text(param)],
            isError: false
        )
    default:
        return .init(content: [.text("Unknown tool: \(params.name)")], isError: true)
    }
}

// Define MCPService to bridge between MCP and ServiceLifecycle
struct MCPService: Service {
    let server: Server
    let transport: Transport

    init(server: Server, transport: Transport) {
        self.server = server
        self.transport = transport
    }

    func run() async throws {
        // Start the server
        try await server.start(transport: transport)

        // Keep running until external cancellation
        // We use a long sleep here, which will be cancelled when the service is shut down
        try await Task.sleep(for: .seconds(31_536_000 * 100)) // 100 years
    }
}

// Create MCP service and other services
// Explicitly use stdio transport
let transport = StdioTransport(logger: logger)
let mcpService = MCPService(server: server, transport: transport)

let serviceGroup = ServiceGroup(
    services: [mcpService],
    gracefulShutdownSignals: [.sigterm, .sigint],
    logger: logger
)

// Run the service group
try await serviceGroup.run()

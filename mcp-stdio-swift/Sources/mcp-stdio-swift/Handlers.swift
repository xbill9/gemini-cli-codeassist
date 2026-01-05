import Foundation
import MCP
import Logging

struct Handlers {
    let logger: Logger

    init(logger: Logger) {
        self.logger = logger
    }

    func listTools(_ params: ListTools.Parameters) async throws -> ListTools.Result {
        let schema: Value = .object([
            "type": .string("object"),
            "properties": .object([
                "param": .object([
                    "type": .string("string"),
                    "description": .string("The name or parameter to greet")
                ])
            ]),
            "required": .array([.string("param")])
        ])
        
        let tools = [
            Tool(
                name: "greet",
                description: "Get a greeting from a local stdio server.",
                inputSchema: schema
            )
        ]
        return .init(tools: tools)
    }

    func callTool(_ params: CallTool.Parameters) async throws -> CallTool.Result {
        switch params.name {
        case "greet":
            logger.debug("Executed greet tool")
            
            guard let param = params.arguments?["param"]?.stringValue else {
                 return .init(content: [.text("Missing required parameter: param")], isError: true)
            }
            
            return .init(
                content: [.text(param)],
                isError: false
            )
        default:
            return .init(content: [.text("Unknown tool: \(params.name)")], isError: true)
        }
    }
}

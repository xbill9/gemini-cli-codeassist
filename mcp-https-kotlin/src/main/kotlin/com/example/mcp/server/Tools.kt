package com.example.mcp.server

import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.types.CallToolResult
import io.modelcontextprotocol.kotlin.sdk.types.TextContent
import io.modelcontextprotocol.kotlin.sdk.types.ToolSchema
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put

object Tools {
    fun configure(server: Server) {
        server.addTool(
            name = Config.Tools.GREET,
            description = "Get a greeting from a local HTTPS server.",
            inputSchema =
                ToolSchema(
                    properties =
                        buildJsonObject {
                            put(
                                Config.Tools.GREET_PARAM,
                                buildJsonObject {
                                    put("type", "string")
                                    put("description", "The name to greet")
                                },
                            )
                        },
                    required = listOf(Config.Tools.GREET_PARAM),
                ),
        ) { request ->
            val param = request.arguments?.get(Config.Tools.GREET_PARAM)
            val name =
                when (param) {
                    is JsonPrimitive -> param.content
                    else -> param?.toString() ?: "World"
                }

            logger.info("Greeting name: $name")
            CallToolResult(content = listOf(TextContent(text = "Hello, $name!")))
        }
    }
}

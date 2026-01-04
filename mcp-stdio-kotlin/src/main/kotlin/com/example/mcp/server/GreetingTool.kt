package com.example.mcp.server

import io.modelcontextprotocol.kotlin.sdk.types.CallToolResult
import io.modelcontextprotocol.kotlin.sdk.types.TextContent
import io.modelcontextprotocol.kotlin.sdk.types.Tool
import io.modelcontextprotocol.kotlin.sdk.types.ToolSchema
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.put
import org.slf4j.LoggerFactory

object GreetingTool {
    private val logger = LoggerFactory.getLogger(GreetingTool::class.java)

    val toolDef =
        Tool(
            name = "greet",
            description = "Get a greeting from a local stdio server.",
            inputSchema =
                ToolSchema(
                    properties =
                        buildJsonObject {
                            put(
                                "param",
                                buildJsonObject {
                                    put("type", "string")
                                    put("description", "The name to greet")
                                },
                            )
                        },
                    required = listOf(),
                ),
        )

    fun handle(arguments: Map<String, JsonElement>?): CallToolResult {
        val paramElement = arguments?.get("param")
        logger.info("Executing greet tool with arguments: {}", arguments)

        // Robustly handle the input
        val nameCandidate =
            when {
                paramElement is JsonPrimitive && paramElement.isString -> paramElement.content
                paramElement is JsonPrimitive -> paramElement.contentOrNull
                else -> paramElement?.toString()
            }

        val name = if (nameCandidate.isNullOrBlank()) "World" else nameCandidate

        val resultString = "Hello, $name!"
        return CallToolResult(content = listOf(TextContent(resultString)))
    }
}

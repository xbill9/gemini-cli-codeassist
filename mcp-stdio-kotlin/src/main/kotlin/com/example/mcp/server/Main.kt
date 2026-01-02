package com.example.mcp.server

import io.modelcontextprotocol.kotlin.sdk.CallToolResult
import io.modelcontextprotocol.kotlin.sdk.Implementation
import io.modelcontextprotocol.kotlin.sdk.TextContent
import io.modelcontextprotocol.kotlin.sdk.Tool
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.ServerCapabilities
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.StdioServerTransport
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import kotlinx.io.asSource
import kotlinx.io.asSink
import kotlinx.io.buffered

fun main() {
    val server = Server(
        serverInfo = Implementation(
            name = "hello-world-server",
            version = "1.0.0"
        ),
        options = ServerOptions(
            capabilities = ServerCapabilities(
                tools = ServerCapabilities.Tools(listChanged = true)
            )
        )
    )

    server.addTool(
        name = "greet",
        description = "Get a greeting from a local stdio server.",
        inputSchema = Tool.Input(
            properties = buildJsonObject {
                put("param", buildJsonObject {
                    put("type", "string")
                    put("description", "The name to greet")
                })
            },
            required = listOf("param")
        )
    ) { request ->
        val param = request.arguments?.get("param")
        val resultString = if (param is JsonPrimitive) {
            param.content
        } else {
            param?.toString()?.replace("\"", "") ?: ""
        }
        
        CallToolResult(content = listOf(TextContent(resultString)))
    }

    val transport = StdioServerTransport(System.`in`.asSource().buffered(), System.out.asSink().buffered())

    runBlocking {
        server.createSession(transport)
        kotlinx.coroutines.awaitCancellation()
    }
}
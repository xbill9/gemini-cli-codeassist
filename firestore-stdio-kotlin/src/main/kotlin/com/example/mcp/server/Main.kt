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
    val firestoreService = FirestoreService()

    val server = Server(
        serverInfo = Implementation(
            name = "inventory-server",
            version = "1.0.0"
        ),
        options = ServerOptions(
            capabilities = ServerCapabilities(
                tools = ServerCapabilities.Tools(listChanged = true)
            )
        )
    )

    // Tool: get_products
    server.addTool(
        name = "get_products",
        description = "Get a list of all products from the inventory database",
        inputSchema = Tool.Input(
            properties = buildJsonObject {},
            required = listOf()
        )
    ) {
        val result = firestoreService.getProducts()
        CallToolResult(content = listOf(TextContent(result)))
    }

    // Tool: get_product_by_id
    server.addTool(
        name = "get_product_by_id",
        description = "Get a single product from the inventory database by its ID",
        inputSchema = Tool.Input(
            properties = buildJsonObject {
                put("id", buildJsonObject {
                    put("type", "string")
                    put("description", "The ID of the product to get")
                })
            },
            required = listOf("id")
        )
    ) { request ->
        val idParam = request.arguments?.get("id")
        val id = if (idParam is JsonPrimitive) {
            idParam.content
        } else {
            idParam?.toString()?.replace("\"", "") ?: ""
        }
        
        val result = firestoreService.getProductById(id)
        CallToolResult(content = listOf(TextContent(result)))
    }

    // Tool: seed
    server.addTool(
        name = "seed",
        description = "Seed the inventory database with products.",
        inputSchema = Tool.Input(
            properties = buildJsonObject {},
            required = listOf()
        )
    ) {
        val result = firestoreService.seed()
        CallToolResult(content = listOf(TextContent(result)))
    }

    // Tool: reset
    server.addTool(
        name = "reset",
        description = "Clears all products from the inventory database.",
        inputSchema = Tool.Input(
            properties = buildJsonObject {},
            required = listOf()
        )
    ) {
        val result = firestoreService.reset()
        CallToolResult(content = listOf(TextContent(result)))
    }
    
    // Tool: get_root (Functionally a tool here to match the registration pattern)
    server.addTool(
        name = "get_root",
        description = "Get a greeting from the Cymbal Superstore Inventory API.",
        inputSchema = Tool.Input(
            properties = buildJsonObject {},
            required = listOf()
        )
    ) {
        CallToolResult(content = listOf(TextContent("🍎 Hello! This is the Cymbal Superstore Inventory API.")))
    }

    // Tool: check_db
    server.addTool(
        name = "check_db",
        description = "Checks if the inventory database is running.",
        inputSchema = Tool.Input(
            properties = buildJsonObject {},
            required = listOf()
        )
    ) {
        val isRunning = firestoreService.checkDb()
        CallToolResult(content = listOf(TextContent("Database running: $isRunning")))
    }

    val transport = StdioServerTransport(System.`in`.asSource().buffered(), System.out.asSink().buffered())

    runBlocking {
        server.createSession(transport)
        kotlinx.coroutines.awaitCancellation()
    }
}
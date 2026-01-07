package com.example.mcp.server

import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.StdioServerTransport
import io.modelcontextprotocol.kotlin.sdk.types.CallToolResult
import io.modelcontextprotocol.kotlin.sdk.types.Implementation
import io.modelcontextprotocol.kotlin.sdk.types.ServerCapabilities
import io.modelcontextprotocol.kotlin.sdk.types.TextContent
import io.modelcontextprotocol.kotlin.sdk.types.ToolSchema
import kotlinx.coroutines.runBlocking
import kotlinx.io.asSink
import kotlinx.io.asSource
import kotlinx.io.buffered
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.slf4j.bridge.SLF4JBridgeHandler

fun main() {
    // Bridge JUL to SLF4J
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val firestoreService = FirestoreService()
    val json = Json { prettyPrint = true }

    val server =
        Server(
            serverInfo =
                Implementation(
                    name = "inventory-server",
                    version = "1.0.0",
                ),
            options =
                ServerOptions(
                    capabilities =
                        ServerCapabilities(
                            tools = ServerCapabilities.Tools(listChanged = true),
                        ),
                ),
        )

    // Tool: get_products
    server.addTool(
        name = "get_products",
        description = "Get a list of all products from the inventory database",
        inputSchema =
            ToolSchema(
                properties = buildJsonObject {},
                required = listOf(),
            ),
    ) {
        val result = firestoreService.getProducts()
        result.fold(
            onSuccess = { products ->
                CallToolResult(content = listOf(TextContent(json.encodeToString(products))))
            },
            onFailure = { e ->
                CallToolResult(
                    content = listOf(TextContent("Error retrieving products: ${e.message}")),
                    isError = true,
                )
            },
        )
    }

    // Tool: get_product_by_id
    server.addTool(
        name = "get_product_by_id",
        description = "Get a single product from the inventory database by its ID",
        inputSchema =
            ToolSchema(
                properties =
                    buildJsonObject {
                        put(
                            "id",
                            buildJsonObject {
                                put("type", "string")
                                put("description", "The ID of the product to get")
                            },
                        )
                    },
                required = listOf("id"),
            ),
    ) { request ->
        val idParam = request.arguments?.get("id")
        val id =
            if (idParam is JsonPrimitive) {
                idParam.content
            } else {
                idParam?.toString()?.replace("\"", "") ?: ""
            }

        val result = firestoreService.getProductById(id)
        result.fold(
            onSuccess = { product ->
                CallToolResult(content = listOf(TextContent(json.encodeToString(product))))
            },
            onFailure = { e ->
                CallToolResult(
                    content = listOf(TextContent("Error retrieving product: ${e.message}")),
                    isError = true,
                )
            },
        )
    }

    // Tool: seed
    server.addTool(
        name = "seed",
        description = "Seed the inventory database with products.",
        inputSchema =
            ToolSchema(
                properties = buildJsonObject {},
                required = listOf(),
            ),
    ) {
        val result = firestoreService.seed()
        result.fold(
            onSuccess = {
                CallToolResult(content = listOf(TextContent("Database seeded successfully.")))
            },
            onFailure = { e ->
                CallToolResult(
                    content = listOf(TextContent("Error seeding database: ${e.message}")),
                    isError = true,
                )
            },
        )
    }

    // Tool: reset
    server.addTool(
        name = "reset",
        description = "Clears all products from the inventory database.",
        inputSchema =
            ToolSchema(
                properties = buildJsonObject {},
                required = listOf(),
            ),
    ) {
        val result = firestoreService.reset()
        result.fold(
            onSuccess = {
                CallToolResult(content = listOf(TextContent("Database reset successfully.")))
            },
            onFailure = { e ->
                CallToolResult(
                    content = listOf(TextContent("Error resetting database: ${e.message}")),
                    isError = true,
                )
            },
        )
    }

    // Tool: get_root (Functionally a tool here to match the registration pattern)
    server.addTool(
        name = "get_root",
        description = "Get a greeting from the Cymbal Superstore Inventory API.",
        inputSchema =
            ToolSchema(
                properties = buildJsonObject {},
                required = listOf(),
            ),
    ) {
        CallToolResult(content = listOf(TextContent("🍎 Hello! This is the Cymbal Superstore Inventory API.")))
    }

    // Tool: check_db
    server.addTool(
        name = "check_db",
        description = "Checks if the inventory database is running.",
        inputSchema =
            ToolSchema(
                properties = buildJsonObject {},
                required = listOf(),
            ),
    ) {
        val isRunning = firestoreService.isDbRunning()
        CallToolResult(content = listOf(TextContent("Database running: $isRunning")))
    }

    val transport = StdioServerTransport(System.`in`.asSource().buffered(), System.out.asSink().buffered())

    runBlocking {
        try {
            server.createSession(transport)
            kotlinx.coroutines.awaitCancellation()
        } finally {
            firestoreService.close()
        }
    }
}

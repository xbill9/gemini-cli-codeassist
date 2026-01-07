package com.example.mcp.server

import com.example.mcp.server.repository.FirestoreInventoryRepository
import com.example.mcp.server.repository.InventoryRepository
import io.ktor.http.HttpHeaders
import io.ktor.http.HttpMethod
import io.ktor.http.HttpStatusCode
import io.ktor.serialization.kotlinx.json.json
import io.ktor.server.application.Application
import io.ktor.server.application.call
import io.ktor.server.application.install
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import io.ktor.server.plugins.contentnegotiation.ContentNegotiation
import io.ktor.server.plugins.cors.routing.CORS
import io.ktor.server.response.respond
import io.ktor.server.routing.get
import io.ktor.server.routing.post
import io.ktor.server.routing.routing
import io.ktor.server.sse.SSE
import io.ktor.server.sse.sse
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.SseServerTransport
import io.modelcontextprotocol.kotlin.sdk.types.CallToolResult
import io.modelcontextprotocol.kotlin.sdk.types.Implementation
import io.modelcontextprotocol.kotlin.sdk.types.ServerCapabilities
import io.modelcontextprotocol.kotlin.sdk.types.TextContent
import io.modelcontextprotocol.kotlin.sdk.types.ToolSchema
import kotlinx.coroutines.awaitCancellation
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.concurrent.ConcurrentHashMap

private val logger = LoggerFactory.getLogger("MCPServer")

fun main() {
    val port = System.getenv("PORT")?.toIntOrNull() ?: 8080

    // Initialize Repository
    val repository: InventoryRepository = FirestoreInventoryRepository()

    embeddedServer(Netty, port = port, host = "0.0.0.0") {
        module(repository)
    }.start(wait = true)
}

fun Application.module(repository: InventoryRepository) {
    configureSerialization()
    configureCORS()
    configureMcpServer(repository)
}

fun Application.configureSerialization() {
    install(ContentNegotiation) {
        json(
            Json {
                prettyPrint = true
                ignoreUnknownKeys = true
            },
        )
    }
}

fun Application.configureCORS() {
    install(CORS) {
        anyHost()
        allowMethod(HttpMethod.Options)
        allowMethod(HttpMethod.Post)
        allowMethod(HttpMethod.Get)
        allowMethod(HttpMethod.Delete)
        allowHeader(HttpHeaders.ContentType)
        allowHeader(HttpHeaders.Authorization)
        allowHeader("Mcp-Session-Id")
        exposeHeader("Mcp-Session-Id")
    }
}

/**
 * Helper to execute tool logic safely.
 * Checks if DB is running and handles exceptions.
 */
suspend fun safeToolCall(
    repository: InventoryRepository,
    logger: Logger,
    action: suspend () -> List<TextContent>,
): CallToolResult {
    if (!repository.isDbRunning) {
        return CallToolResult(
            content = listOf(TextContent(text = "Inventory database is not running.")),
            isError = true,
        )
    }
    return try {
        CallToolResult(content = action())
    } catch (e: Exception) {
        logger.error("Error executing tool", e)
        CallToolResult(
            content = listOf(TextContent(text = "Error: ${e.message}")),
            isError = true,
        )
    }
}

fun Application.configureMcpServer(repository: InventoryRepository) {
    install(SSE)

    val server =
        Server(
            serverInfo =
                Implementation(
                    name = "cymbal-inventory",
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

    // Greet Tool
    server.addTool(
        name = "greet",
        description = "A simple greeting tool",
        inputSchema =
            ToolSchema(
                properties =
                    buildJsonObject {
                        put(
                            "name",
                            buildJsonObject {
                                put("type", "string")
                                put("description", "Name to greet")
                            },
                        )
                    },
            ),
    ) { request ->
        val name =
            request.arguments?.get("name")?.let {
                if (it is JsonPrimitive) it.content else it.toString()
            } ?: "World"

        CallToolResult(content = listOf(TextContent(text = "Hello, $name!")))
    }

    // Get Products
    server.addTool(
        name = "get_products",
        description = "Get a list of all products from the inventory database",
        inputSchema = ToolSchema(properties = buildJsonObject { }),
    ) {
        safeToolCall(repository, logger) {
            val products = repository.getAllProducts()
            listOf(TextContent(text = Json.encodeToString(products)))
        }
    }

    // Get Product By ID
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
        val id =
            request.arguments?.get("id")?.let {
                if (it is JsonPrimitive) it.content else it.toString()
            }

        if (id == null) {
            return@addTool CallToolResult(
                content = listOf(TextContent(text = "Missing 'id' parameter.")),
                isError = true,
            )
        }

        safeToolCall(repository, logger) {
            val product = repository.getProductById(id)
            if (product != null) {
                listOf(TextContent(text = Json.encodeToString(product)))
            } else {
                throw Exception("Product not found.")
            }
        }
    }

    // Search
    server.addTool(
        name = "search",
        description = "Search for products in the inventory database by name",
        inputSchema =
            ToolSchema(
                properties =
                    buildJsonObject {
                        put(
                            "query",
                            buildJsonObject {
                                put("type", "string")
                                put("description", "The search query to filter products by name")
                            },
                        )
                    },
                required = listOf("query"),
            ),
    ) { request ->
        val query =
            request.arguments?.get("query")?.let {
                if (it is JsonPrimitive) it.content else it.toString()
            } ?: ""

        safeToolCall(repository, logger) {
            val results = repository.searchProducts(query)
            listOf(TextContent(text = Json.encodeToString(results)))
        }
    }

    // Seed
    server.addTool(
        name = "seed",
        description = "Seed the inventory database with products.",
        inputSchema = ToolSchema(properties = buildJsonObject { }),
    ) {
        safeToolCall(repository, logger) {
            repository.seedDatabase()
            listOf(TextContent(text = "Database seeded successfully."))
        }
    }

    // Reset
    server.addTool(
        name = "reset",
        description = "Clear all products from the inventory database.",
        inputSchema = ToolSchema(properties = buildJsonObject { }),
    ) {
        safeToolCall(repository, logger) {
            repository.resetDatabase()
            listOf(TextContent(text = "Database reset successfully."))
        }
    }

    // Get Root
    server.addTool(
        name = "get_root",
        description = "Get a greeting from the Cymbal Superstore Inventory API.",
        inputSchema = ToolSchema(properties = buildJsonObject { }),
    ) {
        CallToolResult(content = listOf(TextContent(text = "🍎 Hello! This is the Cymbal Superstore Inventory API.")))
    }

    val transports = ConcurrentHashMap<String, SseServerTransport>()

    routing {
        // Status and Health
        get("/status") {
            call.respond(mapOf("message" to "Cymbal Superstore Inventory API is running.", "db" to repository.isDbRunning))
        }
        get("/health") {
            call.respond(
                mapOf("message" to "Healthy", "db" to repository.isDbRunning, "hostname" to (System.getenv("HOSTNAME") ?: "unknown")),
            )
        }

        sse("/sse") {
            val transport = SseServerTransport("/messages", this)
            transports[transport.sessionId] = transport

            logger.info("New session: ${transport.sessionId}")

            try {
                server.createSession(transport)
                // Wait until the SSE session is closed
                awaitCancellation()
            } catch (e: Exception) {
                logger.error("Session error", e)
            } finally {
                transports.remove(transport.sessionId)
                logger.info("Session closed: ${transport.sessionId}")
            }
        }

        post("/messages") {
            val sessionId = call.request.queryParameters["sessionId"]
            logger.info("POST /messages with sessionId: $sessionId")
            if (sessionId == null) {
                call.respond(HttpStatusCode.BadRequest, "Missing sessionId")
                return@post
            }
            val transport = transports[sessionId]
            if (transport == null) {
                logger.warn("No transport found for sessionId: $sessionId. Available: ${transports.keys}")
                call.respond(HttpStatusCode.BadRequest, "Invalid sessionId")
                return@post
            }

            transport.handlePostMessage(call)
        }
    }
}

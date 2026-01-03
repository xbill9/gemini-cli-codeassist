package com.example.mcp.server

import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.sse.*
import io.ktor.sse.*
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.SseServerTransport
import io.modelcontextprotocol.kotlin.sdk.types.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.slf4j.LoggerFactory
import java.util.concurrent.ConcurrentHashMap

import io.ktor.server.plugins.cors.routing.*
import io.ktor.http.*

private val logger = LoggerFactory.getLogger("MCPServer")

fun main() {
    val port = System.getenv("PORT")?.toIntOrNull() ?: 8080
    embeddedServer(Netty, port = port, host = "0.0.0.0", module = Application::module)
        .start(wait = true)
}

fun Application.module() {
    configureSerialization()
    configureCORS()
    configureMcpServer()
}

fun Application.configureSerialization() {
    install(ContentNegotiation) {
        json()
    }
}

fun Application.configureCORS() {
    install(CORS) {
        anyHost()
        allowMethod(HttpMethod.Options)
        allowMethod(HttpMethod.Post)
        allowMethod(HttpMethod.Get)
        allowHeader(HttpHeaders.ContentType)
        allowHeader(HttpHeaders.Authorization)
        anyHost() // Double check if anyHost is enough
    }
}

fun Application.configureMcpServer() {
    install(SSE)

    val server = Server(
        serverInfo = Implementation(
            name = "mcp-https-server",
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
        description = "Get a greeting from a local HTTPS server.",
        inputSchema = ToolSchema(
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
        val name = when (param) {
            is JsonPrimitive -> param.content
            else -> param?.toString() ?: "World"
        }

        logger.info("Greeting name: $name")
        CallToolResult(content = listOf(TextContent(text = "Hello, $name!")))
    }

    val transports = ConcurrentHashMap<String, SseServerTransport>()

    routing {
        sse("/sse") {
            val transport = SseServerTransport("/messages", this)
            transports[transport.sessionId] = transport
            
            logger.info("New session: ${transport.sessionId}")

            try {
                server.createSession(transport)
                // Wait until the SSE session is closed
                kotlinx.coroutines.awaitCancellation()
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
                call.respond(io.ktor.http.HttpStatusCode.BadRequest, "Missing sessionId")
                return@post
            }
            val transport = transports[sessionId]
            if (transport == null) {
                logger.warn("No transport found for sessionId: $sessionId. Available: ${transports.keys}")
                call.respond(io.ktor.http.HttpStatusCode.BadRequest, "Invalid sessionId")
                return@post
            }

            transport.handlePostMessage(call)
        }
    }
}

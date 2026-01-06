package com.example.mcp.server

import io.ktor.http.HttpHeaders
import io.ktor.http.HttpMethod
import io.ktor.serialization.kotlinx.json.json
import io.ktor.server.application.Application
import io.ktor.server.application.call
import io.ktor.server.application.install
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import io.ktor.server.plugins.contentnegotiation.ContentNegotiation
import io.ktor.server.plugins.cors.routing.CORS
import io.ktor.server.routing.post
import io.ktor.server.routing.routing
import io.ktor.server.sse.SSE
import io.ktor.server.sse.sse
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.types.Implementation
import io.modelcontextprotocol.kotlin.sdk.types.ServerCapabilities
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MCPServer")

fun main() {
    val port = System.getenv("PORT")?.toIntOrNull() ?: Config.DEFAULT_PORT
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

    val server =
        Server(
            serverInfo =
                Implementation(
                    name = Config.SERVER_NAME,
                    version = Config.SERVER_VERSION,
                ),
            options =
                ServerOptions(
                    capabilities =
                        ServerCapabilities(
                            tools = ServerCapabilities.Tools(listChanged = true),
                        ),
                ),
        )

    Tools.configure(server)
    val sessionService = SessionService(server)

    routing {
        sse(Config.Routes.SSE_ENDPOINT) {
            sessionService.handleSseSession(this)
        }

        post(Config.Routes.MESSAGES_ENDPOINT) {
            sessionService.handlePostMessage(call)
        }
    }
}

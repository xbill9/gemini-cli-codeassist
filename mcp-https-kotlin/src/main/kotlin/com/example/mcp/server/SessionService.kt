package com.example.mcp.server

import io.ktor.server.application.ApplicationCall
import io.ktor.server.response.respond
import io.ktor.server.sse.ServerSSESession
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.SseServerTransport
import kotlinx.coroutines.awaitCancellation
import java.util.concurrent.ConcurrentHashMap

/**
 * Service to handle MCP sessions over SSE (Server-Sent Events).
 *
 * @property server The MCP server instance.
 */
class SessionService(private val server: Server) {
    private val transports = ConcurrentHashMap<String, SseServerTransport>()

    /**
     * Handles an incoming SSE session.
     * Keeps the connection open until cancellation.
     *
     * @param session The Ktor SSE session.
     */
    suspend fun handleSseSession(session: ServerSSESession) {
        val transport = SseServerTransport(Config.Routes.MESSAGES_ENDPOINT, session)
        transports[transport.sessionId] = transport

        logger.info("New session: ${transport.sessionId}")

        try {
            server.createSession(transport)
            // Wait until the SSE session is closed
            awaitCancellation()
        } catch (e: Exception) {
            logger.error("Session error for ${transport.sessionId}", e)
            throw e
        } finally {
            transports.remove(transport.sessionId)
            logger.info("Session closed: ${transport.sessionId}")
        }
    }

    /**
     * Handles an incoming POST message for an existing session.
     *
     * @param call The Ktor application call.
     */
    suspend fun handlePostMessage(call: ApplicationCall) {
        val sessionId = call.request.queryParameters["sessionId"]
        logger.info("POST /messages with sessionId: $sessionId")

        if (sessionId == null) {
            call.respond(io.ktor.http.HttpStatusCode.BadRequest, "Missing sessionId")
            return
        }

        val transport = transports[sessionId]
        if (transport == null) {
            logger.warn("No transport found for sessionId: $sessionId. Available: ${transports.keys}")
            call.respond(io.ktor.http.HttpStatusCode.BadRequest, "Invalid sessionId")
            return
        }

        try {
            transport.handlePostMessage(call)
        } catch (e: Exception) {
            logger.error("Error handling POST message for session $sessionId", e)
            call.respond(io.ktor.http.HttpStatusCode.InternalServerError, "Error handling message")
        }
    }
}

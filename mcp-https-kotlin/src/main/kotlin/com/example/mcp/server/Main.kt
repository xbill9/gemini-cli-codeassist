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
import io.modelcontextprotocol.kotlin.sdk.CallToolResult
import io.modelcontextprotocol.kotlin.sdk.Implementation
import io.modelcontextprotocol.kotlin.sdk.TextContent
import io.modelcontextprotocol.kotlin.sdk.Tool
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.ServerCapabilities
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.StdioServerTransport
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.io.asSink
import kotlinx.io.asSource
import kotlinx.io.buffered
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

fun main() {
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

    val sessions = ConcurrentHashMap<String, PipedOutputStream>()

    embeddedServer(Netty, port = 8080) {
        install(SSE)
        install(ContentNegotiation) {
            json()
        }

        routing {
            sse("/sse") {
                val sessionId = UUID.randomUUID().toString()
                println("New session: $sessionId")

                // Input for the server (Written to by POST /messages)
                val serverInput = PipedInputStream()
                val serverInputSink = PipedOutputStream(serverInput)
                sessions[sessionId] = serverInputSink

                // Output from the server (Read by this SSE loop)
                val serverOutputSink = PipedOutputStream()
                val serverOutput = PipedInputStream(serverOutputSink)

                // Bridge streams to Transport
                val transport = StdioServerTransport(
                    serverInput.asSource().buffered(),
                    serverOutputSink.asSink().buffered()
                )

                // Start the MCP session in a background coroutine
                val sessionJob = launch(Dispatchers.IO) {
                    try {
                        server.createSession(transport)
                    } catch (e: Exception) {
                        e.printStackTrace()
                    }
                }

                try {
                    // Send endpoint event
                    send(ServerSentEvent(event = "endpoint", data = "/messages?sessionId=$sessionId"))

                    // Read from server output and forward as SSE events
                    val reader = BufferedReader(InputStreamReader(serverOutput))
                    while (true) {
                        val line = withContext(Dispatchers.IO) {
                            reader.readLine() 
                        } ?: break
                        
                        // MCP JSON-RPC messages are sent as "message" events
                        send(ServerSentEvent(event = "message", data = line))
                    }
                } catch (e: Exception) {
                    // Handle disconnect
                } finally {
                    sessions.remove(sessionId)
                    serverInputSink.close()
                    serverOutputSink.close()
                    sessionJob.cancel()
                    println("Session closed: $sessionId")
                }
            }

            post("/messages") {
                val sessionId = call.request.queryParameters["sessionId"]
                if (sessionId == null || !sessions.containsKey(sessionId)) {
                    call.respond(io.ktor.http.HttpStatusCode.BadRequest, "Invalid or missing sessionId")
                    return@post
                }

                val body = call.receiveText()
                val sessionInput = sessions[sessionId]!!
                
                withContext(Dispatchers.IO) {
                    // StdioServerTransport usually expects line-delimited JSON
                    sessionInput.write((body + "\n").toByteArray())
                    sessionInput.flush()
                }

                call.respond(io.ktor.http.HttpStatusCode.OK)
            }
        }
    }.start(wait = true)
}

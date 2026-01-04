package com.example.mcp.server

import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.StdioServerTransport
import io.modelcontextprotocol.kotlin.sdk.types.Implementation
import io.modelcontextprotocol.kotlin.sdk.types.ServerCapabilities
import kotlinx.coroutines.runBlocking
import kotlinx.io.asSink
import kotlinx.io.asSource
import kotlinx.io.buffered
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("com.example.mcp.server.Main")
    logger.info("Starting MCP server...")
    val server =
        Server(
            serverInfo =
                Implementation(
                    name = "hello-world-server",
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

    server.addTool(
        GreetingTool.toolDef,
    ) { request ->
        GreetingTool.handle(request.arguments)
    }

    val transport = StdioServerTransport(System.`in`.asSource().buffered(), System.out.asSink().buffered())

    runBlocking {
        val session = server.createSession(transport)
        val done = kotlinx.coroutines.Job()
        session.onClose {
            done.complete()
        }
        done.join()
    }
}

package com.example.mcp.server

object Config {
    const val SERVER_NAME = "mcp-https-server"
    const val SERVER_VERSION = "1.0.0"
    const val DEFAULT_PORT = 8080

    object Routes {
        const val SSE_ENDPOINT = "/sse"
        const val MESSAGES_ENDPOINT = "/messages"
    }

    object Tools {
        const val GREET = "greet"
        const val GREET_PARAM = "param"
    }
}

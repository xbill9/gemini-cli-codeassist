package com.example.mcp.server

import io.ktor.client.request.*
import io.ktor.http.*
import io.ktor.server.testing.*
import kotlin.test.*

class ApplicationTest {
    @Test
    fun testMessagesMissingSessionId() = testApplication {
        application {
            module()
        }
        val response = client.post("/messages")
        println("Response status: ${response.status}")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }

    @Test
    fun testMessagesInvalidSessionId() = testApplication {
        application {
            module()
        }
        val response = client.post("/messages?sessionId=invalid")
        assertEquals(HttpStatusCode.BadRequest, response.status)
    }
}

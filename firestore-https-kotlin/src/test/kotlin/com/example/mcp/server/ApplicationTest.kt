package com.example.mcp.server

import com.example.mcp.server.repository.InventoryRepository
import io.ktor.client.request.post
import io.ktor.http.HttpStatusCode
import io.ktor.server.testing.testApplication
import io.mockk.every
import io.mockk.mockk
import kotlin.test.Test
import kotlin.test.assertEquals

class ApplicationTest {
    @Test
    fun testMessagesMissingSessionId() =
        testApplication {
            val repository = mockk<InventoryRepository>(relaxed = true)
            every { repository.isDbRunning } returns true

            application {
                module(repository)
            }
            val response = client.post("/messages")
            assertEquals(HttpStatusCode.BadRequest, response.status)
        }

    @Test
    fun testMessagesInvalidSessionId() =
        testApplication {
            val repository = mockk<InventoryRepository>(relaxed = true)
            every { repository.isDbRunning } returns true

            application {
                module(repository)
            }
            val response = client.post("/messages?sessionId=invalid")
            assertEquals(HttpStatusCode.BadRequest, response.status)
        }
}

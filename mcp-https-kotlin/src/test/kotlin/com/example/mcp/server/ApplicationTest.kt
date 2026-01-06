package com.example.mcp.server

import io.ktor.client.request.post
import io.ktor.http.HttpStatusCode
import io.ktor.server.testing.testApplication
import kotlin.test.Test
import kotlin.test.assertEquals

class ApplicationTest {
    private val logger = org.slf4j.LoggerFactory.getLogger(ApplicationTest::class.java)

    @Test
    fun testMessagesMissingSessionId() =
        testApplication {
            application {
                module()
            }
            val response = client.post(Config.Routes.MESSAGES_ENDPOINT)
            logger.info("Response status: ${response.status}")
            assertEquals(HttpStatusCode.BadRequest, response.status)
        }

    @Test
    fun testMessagesInvalidSessionId() =
        testApplication {
            application {
                module()
            }
            val response = client.post("${Config.Routes.MESSAGES_ENDPOINT}?sessionId=invalid")
            assertEquals(HttpStatusCode.BadRequest, response.status)
        }
}

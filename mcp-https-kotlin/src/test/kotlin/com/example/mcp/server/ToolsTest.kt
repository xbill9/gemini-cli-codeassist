package com.example.mcp.server

import kotlin.test.Test
import kotlin.test.assertEquals

class ToolsTest {
    @Test
    fun testFormatGreeting() {
        val name = "Gemini"
        val expected = "Hello, Gemini!"
        assertEquals(expected, Tools.formatGreeting(name))
    }
}

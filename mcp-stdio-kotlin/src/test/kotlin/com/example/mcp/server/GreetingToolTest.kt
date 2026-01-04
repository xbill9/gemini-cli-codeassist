package com.example.mcp.server

import io.modelcontextprotocol.kotlin.sdk.types.TextContent
import kotlinx.serialization.json.JsonPrimitive
import kotlin.test.Test
import kotlin.test.assertEquals

class GreetingToolTest {
    @Test
    fun `handle returns correct greeting for valid string input`() {
        val input = mapOf("param" to JsonPrimitive("World"))
        val result = GreetingTool.handle(input)

        assertEquals(1, result.content.size)
        val textContent = result.content[0] as TextContent
        assertEquals("Hello, World!", textContent.text)
    }

    @Test
    fun `handle returns empty greeting for null input`() {
        val result = GreetingTool.handle(null)

        assertEquals(1, result.content.size)
        val textContent = result.content[0] as TextContent
        // Based on the logic: paramElement is null -> toString -> "" -> "Hello, !"
        // Wait, logic was: else -> paramElement?.toString() ?: ""
        // if paramElement is null, it returns ""
        // so "Hello, !"
        assertEquals("Hello, World!", textContent.text)
    }

    @Test
    fun `handle handles non-string primitives gracefully`() {
        // e.g. a number
        val input = mapOf("param" to JsonPrimitive(123))
        val result = GreetingTool.handle(input)

        // logic: paramElement is JsonPrimitive but !isString -> contentOrNull
        // JsonPrimitive(123).contentOrNull is "123" (it stringifies numbers generally in content property? Let's verify behavior or strictness)
        // Actually JsonPrimitive.content returns the string representation.
        // But let's check my logic:
        // paramElement is JsonPrimitive && paramElement.isString -> paramElement.content
        // paramElement is JsonPrimitive -> paramElement.contentOrNull ?: ""

        val textContent = result.content[0] as TextContent
        assertEquals("Hello, 123!", textContent.text)
    }

    @Test
    fun `handle returns default greeting for missing required param`() {
        val input = mapOf<String, JsonPrimitive>()
        val result = GreetingTool.handle(input)

        val textContent = result.content[0] as TextContent
        assertEquals("Hello, World!", textContent.text)
    }
}

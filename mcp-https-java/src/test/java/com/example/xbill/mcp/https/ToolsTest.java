package com.example.xbill.mcp.https;

import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

class ToolsTest {

    private final Tools tools = new Tools();

    @Test
    void shouldReverseString() {
        var input = "Hello World";
        var result = tools.reverseString(input);
        assertThat(result).isEqualTo("dlroW olleH");
    }

    @Test
    void shouldHandleNullInputForReverseString() {
        var result = tools.reverseString(null);
        assertThat(result).isEqualTo("Invalid input");
    }

    @Test
    void shouldGreetUser() {
        var name = "Alice";
        var result = tools.greet(name);
        assertThat(result).isEqualTo("Hello, Alice!");
    }

    @Test
    void shouldHandleNullOrEmptyInputForGreet() {
        assertThat(tools.greet(null)).isEqualTo("Hello, Stranger!");
        assertThat(tools.greet("")).isEqualTo("Hello, Stranger!");
        assertThat(tools.greet("   ")).isEqualTo("Hello, Stranger!");
    }
}

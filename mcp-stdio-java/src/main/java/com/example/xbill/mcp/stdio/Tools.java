package com.example.xbill.mcp.stdio;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component
public class Tools {

    @Tool(name = "reverseString", description = "Reverses the input string")
    public String reverseString(@ToolParam(description = "The string to reverse") String input) {
        return new StringBuilder(input).reverse().toString();
    }

    @Tool(name = "greet", description = "Greets the user by name")
    public String greet(@ToolParam(description = "The name of the user") String name) {
        return "Hello, " + name + "!";
    }
}

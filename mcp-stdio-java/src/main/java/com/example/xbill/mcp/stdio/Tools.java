package com.example.xbill.mcp.stdio;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

/**
 * Provides tools for the MCP application.
 */
@Component
public class Tools {

  /**
   * Reverses the input string.
   *
   * @param input the string to reverse
   * @return the reversed string, or "Invalid input" if the input is null
   */
  @Tool(name = "reverseString", description = "Reverses the input string")
  public String reverseString(@ToolParam(description = "The string to reverse") String input) {
    if (input == null) {
      return "Invalid input";
    }
    return new StringBuilder(input).reverse().toString();
  }

  /**
   * Greets the user by name.
   *
   * @param name the name of the user
   * @return a greeting string, or "Hello, Stranger!" if the name is null or empty
   */
  @Tool(name = "greet", description = "Greets the user by name")
  public String greet(@ToolParam(description = "The name of the user") String name) {
    if (name == null || name.isBlank()) {
      return "Hello, Stranger!";
    }
    return "Hello, " + name + "!";
  }
}
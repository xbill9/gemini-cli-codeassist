package com.example.xbill.mcp.stdio;

import org.springframework.ai.tool.ToolCallbackProvider;
import org.springframework.ai.tool.method.MethodToolCallbackProvider;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

/**
 * Main entry point for the MCP Studio Application.
 */
@SpringBootApplication
public class Application {

  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }

  @Bean
  public ToolCallbackProvider toolCallbackProvider(Tools tools) {
    return MethodToolCallbackProvider.builder().toolObjects(tools).build();
  }

}
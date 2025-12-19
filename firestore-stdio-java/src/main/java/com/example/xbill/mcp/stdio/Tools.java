package com.example.xbill.mcp.stdio;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.util.List;
import java.util.concurrent.ExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

/**
 * Provides tools for the MCP application.
 */
@Component
public class Tools {

  private static final Logger logger = LoggerFactory.getLogger(Tools.class);
  private final InventoryService inventoryService;
  private final ObjectMapper objectMapper;

  /**
   * Constructs a new Tools instance.
   *
   * @param inventoryService the inventory service
   * @param objectMapper the object mapper
   */
  public Tools(InventoryService inventoryService, ObjectMapper objectMapper) {
    this.inventoryService = inventoryService;
    this.objectMapper = objectMapper;
    this.objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
  }

  /**
   * Get all products from the inventory database.
   */
  @Tool(
      name = "get_products",
      description = "Get a list of all products from the inventory database"
  )
  public String getProducts() {
    if (!inventoryService.isDbRunning()) {
      return "Inventory database is not running.";
    }
    try {
      List<Product> products = inventoryService.getAllProducts();
      return objectMapper.writeValueAsString(products);
    } catch (ExecutionException | InterruptedException | JsonProcessingException e) {
      logger.error("Error getting products", e);
      return "Error retrieving products: " + e.getMessage();
    }
  }

  /**
   * Get a single product from the inventory database by its ID.
   */
  @Tool(
      name = "get_product_by_id",
      description = "Get a single product from the inventory database by its ID"
  )
  public String getProductById(@ToolParam(description = "The ID of the product to get") String id) {
    if (!inventoryService.isDbRunning()) {
      return "Inventory database is not running.";
    }
    try {
      Product product = inventoryService.getProductById(id);
      if (product == null) {
        return "Product not found.";
      }
      return objectMapper.writeValueAsString(product);
    } catch (ExecutionException | InterruptedException | JsonProcessingException e) {
      logger.error("Error getting product by ID", e);
      return "Error retrieving product: " + e.getMessage();
    }
  }

  /**
   * Seed the inventory database with products.
   */
  @Tool(name = "seed", description = "Seed the inventory database with products.")
  public String seed() {
    if (!inventoryService.isDbRunning()) {
      return "Inventory database is not running.";
    }
    try {
      inventoryService.seed();
      return "Database seeded successfully.";
    } catch (ExecutionException | InterruptedException e) {
      logger.error("Error seeding database", e);
      return "Error seeding database: " + e.getMessage();
    }
  }

  /**
   * Clears all products from the inventory database.
   */
  @Tool(name = "reset", description = "Clears all products from the inventory database.")
  public String reset() {
    if (!inventoryService.isDbRunning()) {
      return "Inventory database is not running.";
    }
    try {
      inventoryService.reset();
      return "Database reset successfully.";
    } catch (ExecutionException | InterruptedException e) {
      logger.error("Error resetting database", e);
      return "Error resetting database: " + e.getMessage();
    }
  }

  /**
   * Get a greeting from the Cymbal Superstore Inventory API.
   */
  @Tool(name = "get_root", description = "Get a greeting from the Cymbal Superstore Inventory API.")
  public String getRoot() {
    return "🍎 Hello! This is the Cymbal Superstore Inventory API.";
  }

  /**
   * Checks if the inventory database is running.
   */
  @Tool(name = "check_db", description = "Checks if the inventory database is running.")
  public String checkDb() {
    boolean isRunning = inventoryService.isDbRunning();
    return "Database running: " + isRunning;
  }
}
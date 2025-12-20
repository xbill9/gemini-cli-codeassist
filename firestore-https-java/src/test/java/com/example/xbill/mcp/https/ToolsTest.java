package com.example.xbill.mcp.https;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ToolsTest {

  private InventoryService inventoryService;
  private ObjectMapper objectMapper;
  private Tools tools;

  @BeforeEach
  void setUp() {
    inventoryService = mock(InventoryService.class);
    objectMapper = new ObjectMapper();
    tools = new Tools(inventoryService, objectMapper);
  }

  @Test
  void shouldGetProducts() throws Exception {
    when(inventoryService.isDbRunning()).thenReturn(true);
    when(inventoryService.getAllProducts()).thenReturn(Collections.emptyList());

    String result = tools.getProducts();
    assertThat(result).isEqualTo("[ ]");
  }

  @Test
  void shouldGetProductById() throws Exception {
    when(inventoryService.isDbRunning()).thenReturn(true);
    Product product = new Product("1", "Test Product", 10.0, 5, "img.png", null, null);
    when(inventoryService.getProductById("1")).thenReturn(product);

    String result = tools.getProductById("1");
    assertThat(result).contains("Test Product");
  }

  @Test
  void shouldHandleProductNotFound() throws Exception {
    when(inventoryService.isDbRunning()).thenReturn(true);
    when(inventoryService.getProductById(anyString())).thenReturn(null);

    String result = tools.getProductById("999");
    assertThat(result).isEqualTo("Product not found.");
  }

  @Test
  void shouldReportDbNotRunning() {
    when(inventoryService.isDbRunning()).thenReturn(false);

    assertThat(tools.getProducts()).isEqualTo("Inventory database is not running.");
    assertThat(tools.getProductById("1")).isEqualTo("Inventory database is not running.");
    assertThat(tools.seed()).isEqualTo("Inventory database is not running.");
    assertThat(tools.reset()).isEqualTo("Inventory database is not running.");
  }
}
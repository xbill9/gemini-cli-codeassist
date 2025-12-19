package com.example.xbill.mcp.stdio;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutionException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ToolsTest {

    @Mock
    private InventoryService inventoryService;

    private Tools tools;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        objectMapper = new ObjectMapper();
        tools = new Tools(inventoryService, objectMapper);
    }

    @Test
    void shouldGetProducts() throws Exception {
        when(inventoryService.isDbRunning()).thenReturn(true);
        Product product = new Product("1", "Apple", 1.0, 100, "img.png", new Date(), new Date());
        when(inventoryService.getAllProducts()).thenReturn(List.of(product));

        String result = tools.getProducts();
        
        assertThat(result).contains("Apple");
        assertThat(result).contains("\"id\" : \"1\"");
    }

    @Test
    void shouldGetProductById() throws Exception {
        when(inventoryService.isDbRunning()).thenReturn(true);
        Product product = new Product("1", "Apple", 1.0, 100, "img.png", new Date(), new Date());
        when(inventoryService.getProductById("1")).thenReturn(product);

        String result = tools.getProductById("1");

        assertThat(result).contains("Apple");
    }

    @Test
    void shouldHandleProductNotFound() throws Exception {
        when(inventoryService.isDbRunning()).thenReturn(true);
        when(inventoryService.getProductById("999")).thenReturn(null);

        String result = tools.getProductById("999");

        assertThat(result).isEqualTo("Product not found.");
    }

    @Test
    void shouldSeedDatabase() throws Exception {
        when(inventoryService.isDbRunning()).thenReturn(true);
        doNothing().when(inventoryService).seed();

        String result = tools.seed();

        assertThat(result).isEqualTo("Database seeded successfully.");
    }

    @Test
    void shouldResetDatabase() throws Exception {
        when(inventoryService.isDbRunning()).thenReturn(true);
        doNothing().when(inventoryService).reset();

        String result = tools.reset();

        assertThat(result).isEqualTo("Database reset successfully.");
    }

    @Test
    void shouldCheckDb() {
        when(inventoryService.isDbRunning()).thenReturn(true);
        assertThat(tools.checkDb()).isEqualTo("Database running: true");

        when(inventoryService.isDbRunning()).thenReturn(false);
        assertThat(tools.checkDb()).isEqualTo("Database running: false");
    }

    @Test
    void shouldGetRoot() {
        assertThat(tools.getRoot()).contains("Cymbal Superstore Inventory API");
    }
    
    @Test
    void shouldHandleDbNotRunning() {
        when(inventoryService.isDbRunning()).thenReturn(false);
        assertThat(tools.getProducts()).isEqualTo("Inventory database is not running.");
        assertThat(tools.getProductById("1")).isEqualTo("Inventory database is not running.");
        assertThat(tools.seed()).isEqualTo("Inventory database is not running.");
        assertThat(tools.reset()).isEqualTo("Inventory database is not running.");
    }
}


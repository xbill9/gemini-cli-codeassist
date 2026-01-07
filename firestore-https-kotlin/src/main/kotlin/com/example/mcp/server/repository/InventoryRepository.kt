package com.example.mcp.server.repository

import com.example.mcp.server.model.Product

interface InventoryRepository {
    val isDbRunning: Boolean

    suspend fun getAllProducts(): List<Product>

    suspend fun getProductById(id: String): Product?

    suspend fun searchProducts(query: String): List<Product>

    suspend fun resetDatabase()

    suspend fun seedDatabase()
}

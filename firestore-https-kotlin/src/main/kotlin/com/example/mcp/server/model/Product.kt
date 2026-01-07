package com.example.mcp.server.model

import kotlinx.serialization.Serializable

@Serializable
data class Product(
    val id: String? = null,
    val name: String,
    val price: Double,
    val quantity: Int,
    val imgfile: String,
    val timestamp: String,
    val actualdateadded: String,
)

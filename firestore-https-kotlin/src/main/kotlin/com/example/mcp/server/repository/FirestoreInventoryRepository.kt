package com.example.mcp.server.repository

import com.example.mcp.server.model.Product
import com.google.cloud.firestore.DocumentSnapshot
import com.google.cloud.firestore.Firestore
import com.google.cloud.firestore.FirestoreOptions
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import org.slf4j.LoggerFactory
import java.time.Instant
import java.util.Date
import kotlin.random.Random

class FirestoreInventoryRepository : InventoryRepository {
    private val logger = LoggerFactory.getLogger("InventoryRepository")
    private var firestore: Firestore? = null

    companion object {
        private const val MAX_BATCH_SIZE = 500
    }

    override val isDbRunning: Boolean
        get() = firestore != null

    init {
        try {
            if (System.getenv("GOOGLE_CLOUD_PROJECT") == null) {
                logger.warn("Missing environment variable: GOOGLE_CLOUD_PROJECT")
            }
            firestore = FirestoreOptions.getDefaultInstance().service
            // Simple read to check connection
            firestore?.collection("inventory")?.limit(1)?.get()?.get()
            logger.info("Firestore connected successfully.")
        } catch (e: Exception) {
            logger.error("Error connecting to Firestore", e)
            firestore = null
        }
    }

    private fun docToProduct(doc: DocumentSnapshot): Product {
        try {
            val data = doc.data ?: throw Exception("Document data is empty for ID: ${doc.id}")
            val name = doc.getString("name") ?: ""
            val price =
                try {
                    doc.getDouble("price") ?: 0.0
                } catch (e: Exception) {
                    logger.warn("Error reading price for doc ${doc.id}, attempting to read as Long")
                    doc.getLong("price")?.toDouble() ?: 0.0
                }
            val quantity =
                try {
                    doc.getLong("quantity")?.toInt() ?: 0
                } catch (e: Exception) {
                    logger.warn("Error reading quantity for doc ${doc.id}, attempting to read as Double")
                    doc.getDouble("quantity")?.toInt() ?: 0
                }
            val imgfile = doc.getString("imgfile") ?: ""

            val timestamp =
                try {
                    doc.getDate("timestamp")?.toInstant()?.toString() ?: Instant.now().toString()
                } catch (e: Exception) {
                    logger.warn("Error reading timestamp for doc ${doc.id}, using current time: ${e.message}")
                    Instant.now().toString()
                }
            val actualdateadded =
                try {
                    doc.getDate("actualdateadded")?.toInstant()?.toString() ?: Instant.now().toString()
                } catch (e: Exception) {
                    logger.warn("Error reading actualdateadded for doc ${doc.id}, using current time: ${e.message}")
                    Instant.now().toString()
                }

            return Product(
                id = doc.id,
                name = name,
                price = price,
                quantity = quantity,
                imgfile = imgfile,
                timestamp = timestamp,
                actualdateadded = actualdateadded,
            )
        } catch (e: Exception) {
            logger.error("Failed to convert document ${doc.id} to Product", e)
            throw e
        }
    }

    override suspend fun getAllProducts(): List<Product> =
        withContext(Dispatchers.IO) {
            val db = firestore ?: throw IllegalStateException("Database not connected")
            db.collection("inventory").get().get().documents.map { docToProduct(it) }
        }

    override suspend fun getProductById(id: String): Product? =
        withContext(Dispatchers.IO) {
            val db = firestore ?: throw IllegalStateException("Database not connected")
            val doc = db.collection("inventory").document(id).get().get()
            if (doc != null && doc.exists()) docToProduct(doc) else null
        }

    override suspend fun searchProducts(query: String): List<Product> =
        withContext(Dispatchers.IO) {
            // Firestore doesn't support substring search natively, so we fetch all and filter in memory for this simple example.
            // For production, use a dedicated search service (e.g., Algolia, Elasticsearch).
            getAllProducts().filter { it.name.contains(query, ignoreCase = true) }
        }

    override suspend fun resetDatabase() =
        withContext(Dispatchers.IO) {
            val db = firestore ?: throw IllegalStateException("Database not connected")
            val collectionRef = db.collection("inventory")
            val snapshot = collectionRef.get().get()

            if (snapshot.isEmpty) return@withContext

            val docs = snapshot.documents

            for (i in docs.indices step MAX_BATCH_SIZE) {
                val batch = db.batch()
                val end = minOf(i + MAX_BATCH_SIZE, docs.size)
                val chunk = docs.subList(i, end)
                chunk.forEach { doc -> batch.delete(doc.reference) }
                batch.commit().get()
            }
        }

    override suspend fun seedDatabase() =
        withContext(Dispatchers.IO) {
            val oldProducts =
                listOf(
                    "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
                    "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
                    "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
                    "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
                    "Sunflower Seeds", "Fresh Basil", "Cinnamon",
                )

            for (productName in oldProducts) {
                val oldProduct =
                    Product(
                        name = productName,
                        price = Random.nextInt(1, 11).toDouble(),
                        quantity = Random.nextInt(1, 501),
                        imgfile = "product-images/${productName.replace("\\s".toRegex(), "").lowercase()}.png",
                        timestamp = Instant.now().minusMillis(Random.nextLong(0, 31536000000L) + 7776000000L).toString(),
                        actualdateadded = Instant.now().toString(),
                    )
                logger.info("⬆️ Adding (or updating) product in firestore: ${oldProduct.name}")
                addOrUpdateFirestore(oldProduct)
            }

            val recentProducts =
                listOf(
                    "Parmesan Crisps",
                    "Pineapple Kombucha",
                    "Maple Almond Butter",
                    "Mint Chocolate Cookies",
                    "White Chocolate Caramel Corn",
                    "Acai Smoothie Packs",
                    "Smores Cereal",
                    "Peanut Butter and Jelly Cups",
                )

            for (productName in recentProducts) {
                val recent =
                    Product(
                        name = productName,
                        price = Random.nextInt(1, 11).toDouble(),
                        quantity = Random.nextInt(1, 101),
                        imgfile = "product-images/${productName.replace("\\s".toRegex(), "").lowercase()}.png",
                        timestamp = Instant.now().minusMillis(Random.nextLong(0, 518400000L) + 1L).toString(),
                        actualdateadded = Instant.now().toString(),
                    )
                logger.info("🆕 Adding (or updating) product in firestore: ${recent.name}")
                addOrUpdateFirestore(recent)
            }

            val recentProductsOutOfStock = listOf("Wasabi Party Mix", "Jalapeno Seasoning")
            for (productName in recentProductsOutOfStock) {
                val oosProduct =
                    Product(
                        name = productName,
                        price = Random.nextInt(1, 11).toDouble(),
                        quantity = 0,
                        imgfile = "product-images/${productName.replace("\\s".toRegex(), "").lowercase()}.png",
                        timestamp = Instant.now().minusMillis(Random.nextLong(0, 518400000L) + 1L).toString(),
                        actualdateadded = Instant.now().toString(),
                    )
                logger.info("😱 Adding (or updating) out of stock product in firestore: ${oosProduct.name}")
                addOrUpdateFirestore(oosProduct)
            }
        }

    private suspend fun addOrUpdateFirestore(product: Product) {
        val db = firestore ?: return
        val querySnapshot =
            db.collection("inventory")
                .whereEqualTo("name", product.name)
                .get()
                .get()

        val productMap =
            mapOf(
                "name" to product.name,
                "price" to product.price,
                "quantity" to product.quantity,
                "imgfile" to product.imgfile,
                "timestamp" to
                    if (product.timestamp.isNotEmpty()) {
                        Date.from(Instant.parse(product.timestamp))
                    } else {
                        Date()
                    },
                "actualdateadded" to
                    if (product.actualdateadded.isNotEmpty()) {
                        Date.from(Instant.parse(product.actualdateadded))
                    } else {
                        Date()
                    },
            )

        if (querySnapshot.isEmpty) {
            db.collection("inventory").add(productMap).get()
        } else {
            for (doc in querySnapshot.documents) {
                db.collection("inventory").document(doc.id).update(productMap).get()
            }
        }
    }
}

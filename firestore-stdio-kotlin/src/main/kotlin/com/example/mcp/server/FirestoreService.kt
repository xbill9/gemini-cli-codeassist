package com.example.mcp.server

import com.google.auth.oauth2.GoogleCredentials
import com.google.cloud.firestore.Firestore
import com.google.cloud.firestore.FirestoreOptions
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encodeToString
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.Json
import org.slf4j.LoggerFactory
import java.time.Instant
import java.util.Date

// Custom serializer to handle Date as ISO-8601 strings
object DateSerializer : KSerializer<Date> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("Date", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: Date) {
        encoder.encodeString(value.toInstant().toString())
    }

    override fun deserialize(decoder: Decoder): Date {
        return Date.from(Instant.parse(decoder.decodeString()))
    }
}

@Serializable
data class Product(
    val id: String? = null,
    val name: String,
    val price: Int,
    val quantity: Int,
    val imgfile: String,
    @Serializable(with = DateSerializer::class)
    val timestamp: Date,
    @Serializable(with = DateSerializer::class)
    val actualdateadded: Date
)

class FirestoreService {
    private val logger = LoggerFactory.getLogger(FirestoreService::class.java)
    private var firestore: Firestore? = null
    var dbRunning: Boolean = false

    private val json = Json {
        prettyPrint = true
        ignoreUnknownKeys = true
    }

    init {
        try {
            // Assumes GOOGLE_APPLICATION_CREDENTIALS or default auth is set
            val options = FirestoreOptions.getDefaultInstance().toBuilder()
                .build()
            firestore = options.service
            dbRunning = true
            logger.info("Firestore client initialized")
        } catch (e: Exception) {
            logger.error("Failed to initialize Firestore", e)
            dbRunning = false
        }
    }

    fun getProducts(): String {
        if (!dbRunning || firestore == null) return "Inventory database is not running."
        
        return try {
            val future = firestore!!.collection("inventory").get()
            val documents = future.get().documents
            val products = documents.map { docToProduct(it) }
            json.encodeToString(products)
        } catch (e: Exception) {
            logger.error("Error getting products", e)
            "Error retrieving products: ${e.message}"
        }
    }

    fun getProductById(id: String): String {
        if (!dbRunning || firestore == null) return "Inventory database is not running."

        return try {
            val future = firestore!!.collection("inventory").document(id).get()
            val document = future.get()
            if (document.exists()) {
                val product = docToProduct(document, document.id)
                json.encodeToString(product)
            } else {
                "Product not found."
            }
        } catch (e: Exception) {
            logger.error("Error getting product by id", e)
            "Error retrieving product: ${e.message}"
        }
    }

    fun seed(): String {
        if (!dbRunning || firestore == null) return "Inventory database is not running."
        
        return try {
            initFirestoreCollection()
            "Database seeded successfully."
        } catch (e: Exception) {
            logger.error("Error seeding database", e)
            "Error seeding database: ${e.message}"
        }
    }

    fun reset(): String {
        if (!dbRunning || firestore == null) return "Inventory database is not running."

        return try {
            cleanFirestoreCollection()
            "Database reset successfully."
        } catch (e: Exception) {
            logger.error("Error resetting database", e)
            "Error resetting database: ${e.message}"
        }
    }
    
    fun checkDb(): Boolean {
        return dbRunning
    }

    private fun docToProduct(doc: com.google.cloud.firestore.DocumentSnapshot, idOverride: String? = null): Product {
        val name = doc.getString("name") ?: ""
        val price = doc.getLong("price")?.toInt() ?: 0
        val quantity = doc.getLong("quantity")?.toInt() ?: 0
        val imgfile = doc.getString("imgfile") ?: ""
        
        val timestamp = doc.getDate("timestamp") ?: Date()
        val actualdateadded = doc.getDate("actualdateadded") ?: Date()

        return Product(
            id = idOverride ?: doc.id,
            name = name,
            price = price,
            quantity = quantity,
            imgfile = imgfile,
            timestamp = timestamp,
            actualdateadded = actualdateadded
        )
    }

    private fun initFirestoreCollection() {
        val oldProducts = listOf(
            "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
            "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
            "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
            "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
            "Sunflower Seeds", "Fresh Basil", "Cinnamon"
        )

        for (productName in oldProducts) {
            val product = createRandomProduct(productName, isOld = true)
            logger.info("⬆️ Adding (or updating) product in firestore: ${product.name}")
            addOrUpdateFirestore(product)
        }

        val recentProducts = listOf(
            "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
            "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
            "Smores Cereal", "Peanut Butter and Jelly Cups"
        )

        for (productName in recentProducts) {
            val product = createRandomProduct(productName, isOld = false)
            logger.info("🆕 Adding (or updating) product in firestore: ${product.name}")
            addOrUpdateFirestore(product)
        }

        val recentProductsOutOfStock = listOf("Wasabi Party Mix", "Jalapeno Seasoning")
        for (productName in recentProductsOutOfStock) {
            val product = createRandomProduct(productName, isOld = false, outOfStock = true)
            logger.info("😱 Adding (or updating) out of stock product in firestore: ${product.name}")
            addOrUpdateFirestore(product)
        }
    }

    private fun createRandomProduct(name: String, isOld: Boolean, outOfStock: Boolean = false): Product {
        val price = (Math.random() * 10 + 1).toInt()
        val quantity = if (outOfStock) 0 else (Math.random() * 500 + 1).toInt()
        val imgfile = "product-images/${name.replace("\\s".toRegex(), "").lowercase()}.png"
        
        val now = System.currentTimeMillis()
        val timestampMillis = if (isOld) {
             now - (Math.random() * 31536000000).toLong() - 7776000000L
        } else {
             now - (Math.random() * 518400000).toLong() + 1
        }

        return Product(
            name = name,
            price = price,
            quantity = quantity,
            imgfile = imgfile,
            timestamp = Date(timestampMillis),
            actualdateadded = Date(now)
        )
    }

    private fun addOrUpdateFirestore(product: Product) {
        val collection = firestore!!.collection("inventory")
        val query = collection.whereEqualTo("name", product.name).get().get()

        val productMap = mapOf(
            "name" to product.name,
            "price" to product.price,
            "quantity" to product.quantity,
            "imgfile" to product.imgfile,
            "timestamp" to product.timestamp,
            "actualdateadded" to product.actualdateadded
        )

        if (query.isEmpty) {
            collection.add(productMap).get()
        } else {
            for (doc in query.documents) {
                collection.document(doc.id).update(productMap).get()
            }
        }
    }

    private fun cleanFirestoreCollection() {
        logger.info("Cleaning Firestore collection...")
        val collection = firestore!!.collection("inventory")
        val snapshot = collection.get().get()
        
        if (!snapshot.isEmpty) {
            val batch = firestore!!.batch()
            for (doc in snapshot.documents) {
                batch.delete(doc.reference)
            }
            batch.commit().get()
        }
        logger.info("Firestore collection cleaned.")
    }
}
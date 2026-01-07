package com.example.mcp.server

import com.google.api.core.ApiFuture
import com.google.api.core.ApiFutureCallback
import com.google.api.core.ApiFutures
import com.google.cloud.firestore.Firestore
import com.google.cloud.firestore.FirestoreOptions
import com.google.common.util.concurrent.MoreExecutors
import kotlinx.coroutines.suspendCancellableCoroutine
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import org.slf4j.LoggerFactory
import java.time.Instant
import java.util.Date
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException

// Extension to allow awaiting ApiFuture in coroutines
suspend fun <T> ApiFuture<T>.await(): T =
    suspendCancellableCoroutine { cont ->
        ApiFutures.addCallback(
            this,
            object : ApiFutureCallback<T> {
                override fun onSuccess(result: T?) {
                    @Suppress("UNCHECKED_CAST")
                    cont.resume(result as T)
                }

                override fun onFailure(t: Throwable) {
                    cont.resumeWithException(t)
                }
            },
            MoreExecutors.directExecutor(),
        )
    }

// Custom serializer to handle Date as ISO-8601 strings
object DateSerializer : KSerializer<Date> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("Date", PrimitiveKind.STRING)

    override fun serialize(
        encoder: Encoder,
        value: Date,
    ) {
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
    val actualdateadded: Date,
)

class FirestoreService(firestoreInstance: Firestore? = null) : AutoCloseable {
    private val logger = LoggerFactory.getLogger(FirestoreService::class.java)
    private val firestore: Firestore?

    init {
        firestore =
            if (firestoreInstance != null) {
                firestoreInstance
            } else {
                try {
                    // Assumes GOOGLE_APPLICATION_CREDENTIALS or default auth is set
                    val options =
                        FirestoreOptions.getDefaultInstance().toBuilder()
                            .build()
                    logger.info("Firestore client initialized")
                    options.service
                } catch (e: Exception) {
                    logger.error("Failed to initialize Firestore", e)
                    null
                }
            }
    }

    override fun close() {
        try {
            firestore?.close()
        } catch (e: Exception) {
            logger.error("Error closing Firestore client", e)
        }
    }

    private fun checkDb(): Firestore {
        return firestore ?: throw IllegalStateException("Inventory database is not running.")
    }

    fun isDbRunning(): Boolean = firestore != null

    suspend fun getProducts(): Result<List<Product>> =
        runCatching {
            val db = checkDb()
            val snapshot = db.collection("inventory").get().await()
            snapshot.documents.map { docToProduct(it) }
        }

    suspend fun getProductById(id: String): Result<Product> =
        runCatching {
            val db = checkDb()
            val document = db.collection("inventory").document(id).get().await()
            if (document.exists()) {
                docToProduct(document, document.id)
            } else {
                throw NoSuchElementException("Product not found.")
            }
        }

    suspend fun seed(): Result<Unit> =
        runCatching {
            checkDb()
            initFirestoreCollection()
        }

    suspend fun reset(): Result<Unit> =
        runCatching {
            checkDb()
            cleanFirestoreCollection()
        }

    private fun docToProduct(
        doc: com.google.cloud.firestore.DocumentSnapshot,
        idOverride: String? = null,
    ): Product {
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
            actualdateadded = actualdateadded,
        )
    }

    private suspend fun initFirestoreCollection() {
        val oldProducts =
            listOf(
                "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
                "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
                "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
                "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
                "Sunflower Seeds", "Fresh Basil", "Cinnamon",
            )

        for (productName in oldProducts) {
            val product = createRandomProduct(productName, isOld = true)
            logger.info("⬆️ Adding (or updating) product in firestore: ${product.name}")
            addOrUpdateFirestore(product)
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

    private fun createRandomProduct(
        name: String,
        isOld: Boolean,
        outOfStock: Boolean = false,
    ): Product {
        val price = (Math.random() * 10 + 1).toInt()
        val quantity = if (outOfStock) 0 else (Math.random() * 500 + 1).toInt()
        val imgfile = "product-images/${name.replace("\\s".toRegex(), "").lowercase()}.png"

        val now = System.currentTimeMillis()
        val timestampMillis =
            if (isOld) {
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
            actualdateadded = Date(now),
        )
    }

    private suspend fun addOrUpdateFirestore(product: Product) {
        val db = checkDb()
        val collection = db.collection("inventory")
        val query = collection.whereEqualTo("name", product.name).get().await()

        val productMap =
            mapOf(
                "name" to product.name,
                "price" to product.price,
                "quantity" to product.quantity,
                "imgfile" to product.imgfile,
                "timestamp" to product.timestamp,
                "actualdateadded" to product.actualdateadded,
            )

        if (query.isEmpty()) {
            collection.add(productMap).await()
        } else {
            for (doc in query.documents) {
                collection.document(doc.id).update(productMap).await()
            }
        }
    }

    private suspend fun cleanFirestoreCollection() {
        logger.info("Cleaning Firestore collection...")
        val db = checkDb()
        val collection = db.collection("inventory")
        val snapshot = collection.get().await()

        if (!snapshot.isEmpty()) {
            val batch = db.batch()
            for (doc in snapshot.documents) {
                batch.delete(doc.reference)
            }
            batch.commit().await()
        }
        logger.info("Firestore collection cleaned.")
    }
}

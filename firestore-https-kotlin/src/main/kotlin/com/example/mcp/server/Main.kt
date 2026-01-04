package com.example.mcp.server

import com.google.cloud.firestore.DocumentSnapshot
import com.google.cloud.firestore.Firestore
import com.google.cloud.firestore.FirestoreOptions
import io.ktor.http.*
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.server.plugins.cors.routing.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.sse.*
import io.ktor.sse.*
import io.modelcontextprotocol.kotlin.sdk.server.Server
import io.modelcontextprotocol.kotlin.sdk.server.ServerOptions
import io.modelcontextprotocol.kotlin.sdk.server.SseServerTransport
import io.modelcontextprotocol.kotlin.sdk.types.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.slf4j.LoggerFactory
import java.time.Instant
import java.util.Date
import java.util.concurrent.ConcurrentHashMap
import kotlin.random.Random

private val logger = LoggerFactory.getLogger("MCPServer")
private var firestore: Firestore? = null
private var dbRunning: Boolean = false

@Serializable
data class Product(
    val id: String? = null,
    val name: String,
    val price: Double,
    val quantity: Int,
    val imgfile: String,
    val timestamp: String,
    val actualdateadded: String
)

fun main() {
    val port = System.getenv("PORT")?.toIntOrNull() ?: 8080

    // Initialize Firestore
    try {
        if (System.getenv("GOOGLE_CLOUD_PROJECT") == null) {
            logger.warn("Missing environment variable: GOOGLE_CLOUD_PROJECT")
        }
        firestore = FirestoreOptions.getDefaultInstance().service
        // Simple read to check connection
        firestore?.collection("inventory")?.limit(1)?.get()?.get()
        dbRunning = true
        logger.info("Firestore connected successfully.")
    } catch (e: Exception) {
        logger.error("Error connecting to Firestore", e)
        dbRunning = false
    }

    embeddedServer(Netty, port = port, host = "0.0.0.0", module = Application::module)
        .start(wait = true)
}

fun Application.module() {
    configureSerialization()
    configureCORS()
    configureMcpServer()
}

fun Application.configureSerialization() {
    install(ContentNegotiation) {
        json(Json {
            prettyPrint = true
            ignoreUnknownKeys = true
        })
    }
}

fun Application.configureCORS() {
    install(CORS) {
        anyHost()
        allowMethod(HttpMethod.Options)
        allowMethod(HttpMethod.Post)
        allowMethod(HttpMethod.Get)
        allowMethod(HttpMethod.Delete)
        allowHeader(HttpHeaders.ContentType)
        allowHeader(HttpHeaders.Authorization)
        allowHeader("Mcp-Session-Id")
        exposeHeader("Mcp-Session-Id")
    }
}

fun docToProduct(doc: DocumentSnapshot): Product {
    try {
        val data = doc.data ?: throw Exception("Document data is empty for ID: ${doc.id}")
        val name = doc.getString("name") ?: ""
        val price = try { doc.getDouble("price") ?: 0.0 } catch (e: Exception) { 
            logger.warn("Error reading price for doc ${doc.id}, attempting to read as Long")
            doc.getLong("price")?.toDouble() ?: 0.0 
        }
        val quantity = try { doc.getLong("quantity")?.toInt() ?: 0 } catch (e: Exception) {
             logger.warn("Error reading quantity for doc ${doc.id}, attempting to read as Double")
             doc.getDouble("quantity")?.toInt() ?: 0
        }
        val imgfile = doc.getString("imgfile") ?: ""
        
        val timestamp = try { doc.getDate("timestamp")?.toInstant()?.toString() ?: Instant.now().toString() } catch (e: Exception) {
            logger.warn("Error reading timestamp for doc ${doc.id}, using current time: ${e.message}")
            Instant.now().toString()
        }
        val actualdateadded = try { doc.getDate("actualdateadded")?.toInstant()?.toString() ?: Instant.now().toString() } catch (e: Exception) {
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
            actualdateadded = actualdateadded
        )
    } catch (e: Exception) {
        logger.error("Failed to convert document ${doc.id} to Product", e)
        throw e
    }
}

suspend fun cleanFirestoreCollection() = withContext(Dispatchers.IO) {
    val db = firestore ?: return@withContext
    val collectionRef = db.collection("inventory")
    val snapshot = collectionRef.get().get()
    
    if (snapshot.isEmpty) return@withContext

    val MAX_BATCH_SIZE = 500
    val docs = snapshot.documents
    
    for (i in docs.indices step MAX_BATCH_SIZE) {
        val batch = db.batch()
        val end = minOf(i + MAX_BATCH_SIZE, docs.size)
        val chunk = docs.subList(i, end)
        chunk.forEach { doc -> batch.delete(doc.reference) }
        batch.commit().get()
    }
}

suspend fun addOrUpdateFirestore(product: Product) = withContext(Dispatchers.IO) {
    val db = firestore ?: return@withContext
    val querySnapshot = db.collection("inventory")
        .whereEqualTo("name", product.name)
        .get()
        .get()

    val productMap = mapOf(
        "name" to product.name,
        "price" to product.price,
        "quantity" to product.quantity,
        "imgfile" to product.imgfile,
        "timestamp" to if (product.timestamp.isNotEmpty()) Date.from(Instant.parse(product.timestamp)) else Date(),
        "actualdateadded" to if (product.actualdateadded.isNotEmpty()) Date.from(Instant.parse(product.actualdateadded)) else Date()
    )

    if (querySnapshot.isEmpty) {
        db.collection("inventory").add(productMap).get()
    } else {
        for (doc in querySnapshot.documents) {
            db.collection("inventory").document(doc.id).update(productMap).get()
        }
    }
}

suspend fun initFirestoreCollection() {
    val oldProducts = listOf(
        "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
        "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
        "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
        "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
        "Sunflower Seeds", "Fresh Basil", "Cinnamon"
    )

    for (productName in oldProducts) {
        val oldProduct = Product(
            name = productName,
            price = Random.nextInt(1, 11).toDouble(),
            quantity = Random.nextInt(1, 501),
            imgfile = "product-images/${productName.replace("\\s".toRegex(), "").lowercase()}.png",
            timestamp = Instant.now().minusMillis(Random.nextLong(0, 31536000000L) + 7776000000L).toString(),
            actualdateadded = Instant.now().toString()
        )
        logger.info("⬆️ Adding (or updating) product in firestore: ${oldProduct.name}")
        addOrUpdateFirestore(oldProduct)
    }

    val recentProducts = listOf(
        "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
        "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
        "Smores Cereal", "Peanut Butter and Jelly Cups"
    )

    for (productName in recentProducts) {
        val recent = Product(
            name = productName,
            price = Random.nextInt(1, 11).toDouble(),
            quantity = Random.nextInt(1, 101),
            imgfile = "product-images/${productName.replace("\\s".toRegex(), "").lowercase()}.png",
            timestamp = Instant.now().minusMillis(Random.nextLong(0, 518400000L) + 1L).toString(),
            actualdateadded = Instant.now().toString()
        )
        logger.info("🆕 Adding (or updating) product in firestore: ${recent.name}")
        addOrUpdateFirestore(recent)
    }

    val recentProductsOutOfStock = listOf("Wasabi Party Mix", "Jalapeno Seasoning")
    for (productName in recentProductsOutOfStock) {
        val oosProduct = Product(
            name = productName,
            price = Random.nextInt(1, 11).toDouble(),
            quantity = 0,
            imgfile = "product-images/${productName.replace("\\s".toRegex(), "").lowercase()}.png",
            timestamp = Instant.now().minusMillis(Random.nextLong(0, 518400000L) + 1L).toString(),
            actualdateadded = Instant.now().toString()
        )
        logger.info("😱 Adding (or updating) out of stock product in firestore: ${oosProduct.name}")
        addOrUpdateFirestore(oosProduct)
    }
}

fun Application.configureMcpServer() {
    install(SSE)

    val server = Server(
        serverInfo = Implementation(
            name = "cymbal-inventory",
            version = "1.0.0"
        ),
        options = ServerOptions(
            capabilities = ServerCapabilities(
                tools = ServerCapabilities.Tools(listChanged = true)
            )
        )
    )

    // Greet Tool
    server.addTool(
        name = "greet",
        description = "A simple greeting tool",
        inputSchema = ToolSchema(
            properties = buildJsonObject {
                put("name", buildJsonObject {
                    put("type", "string")
                    put("description", "Name to greet")
                })
            }
        )
    ) { request ->
        val name = request.arguments?.get("name")?.let {
            if (it is JsonPrimitive) it.content else it.toString()
        } ?: "World"

        CallToolResult(content = listOf(TextContent(text = "Hello, $name!")))
    }

    // Get Products
    server.addTool(
        name = "get_products",
        description = "Get a list of all products from the inventory database",
        inputSchema = ToolSchema(properties = buildJsonObject { })
    ) {
        if (!dbRunning) {
            return@addTool CallToolResult(
                content = listOf(TextContent(text = "Inventory database is not running.")),
                isError = true
            )
        }
        try {
            val products = withContext(Dispatchers.IO) {
                firestore?.collection("inventory")?.get()?.get()?.documents?.map { docToProduct(it) } ?: emptyList()
            }
            CallToolResult(content = listOf(TextContent(text = Json.encodeToString(products))))
        } catch (e: Exception) {
            logger.error("Error fetching products", e)
            CallToolResult(
                content = listOf(TextContent(text = "Error fetching products: ${e.message}")),
                isError = true
            )
        }
    }

    // Get Product By ID
    server.addTool(
        name = "get_product_by_id",
        description = "Get a single product from the inventory database by its ID",
        inputSchema = ToolSchema(
            properties = buildJsonObject {
                put("id", buildJsonObject {
                    put("type", "string")
                    put("description", "The ID of the product to get")
                })
            },
            required = listOf("id")
        )
    ) { request ->
        val id = request.arguments?.get("id")?.let {
            if (it is JsonPrimitive) it.content else it.toString()
        }
        
        if (id == null) {
             return@addTool CallToolResult(
                content = listOf(TextContent(text = "Missing 'id' parameter.")),
                isError = true
            )
        }

        if (!dbRunning) {
            return@addTool CallToolResult(
                content = listOf(TextContent(text = "Inventory database is not running.")),
                isError = true
            )
        }
        try {
            val product = withContext(Dispatchers.IO) {
                val doc = firestore?.collection("inventory")?.document(id)?.get()?.get()
                if (doc != null && doc.exists()) docToProduct(doc) else null
            }
            
            if (product != null) {
                CallToolResult(content = listOf(TextContent(text = Json.encodeToString(product))))
            } else {
                CallToolResult(
                    content = listOf(TextContent(text = "Product not found.")),
                    isError = true
                )
            }
        } catch (e: Exception) {
            logger.error("Error fetching product $id", e)
            CallToolResult(
                content = listOf(TextContent(text = "Error fetching product.")),
                isError = true
            )
        }
    }

    // Search
    server.addTool(
        name = "search",
        description = "Search for products in the inventory database by name",
        inputSchema = ToolSchema(
            properties = buildJsonObject {
                put("query", buildJsonObject {
                    put("type", "string")
                    put("description", "The search query to filter products by name")
                })
            },
            required = listOf("query")
        )
    ) { request ->
        val query = request.arguments?.get("query")?.let {
            if (it is JsonPrimitive) it.content else it.toString()
        } ?: ""

        if (!dbRunning) {
            return@addTool CallToolResult(
                content = listOf(TextContent(text = "Inventory database is not running.")),
                isError = true
            )
        }
        try {
            val products = withContext(Dispatchers.IO) {
                firestore?.collection("inventory")?.get()?.get()?.documents?.map { docToProduct(it) } ?: emptyList()
            }
            val results = products.filter { it.name.contains(query, ignoreCase = true) }
            CallToolResult(content = listOf(TextContent(text = Json.encodeToString(results))))
        } catch (e: Exception) {
            logger.error("Error searching products", e)
            CallToolResult(
                content = listOf(TextContent(text = "Error searching products.")),
                isError = true
            )
        }
    }

    // Seed
    server.addTool(
        name = "seed",
        description = "Seed the inventory database with products.",
        inputSchema = ToolSchema(properties = buildJsonObject { })
    ) {
        if (!dbRunning) {
            return@addTool CallToolResult(
                content = listOf(TextContent(text = "Inventory database is not running.")),
                isError = true
            )
        }
        initFirestoreCollection()
        CallToolResult(content = listOf(TextContent(text = "Database seeded successfully.")))
    }

    // Reset
    server.addTool(
        name = "reset",
        description = "Clear all products from the inventory database.",
        inputSchema = ToolSchema(properties = buildJsonObject { })
    ) {
        if (!dbRunning) {
            return@addTool CallToolResult(
                content = listOf(TextContent(text = "Inventory database is not running.")),
                isError = true
            )
        }
        cleanFirestoreCollection()
        CallToolResult(content = listOf(TextContent(text = "Database reset successfully.")))
    }

    // Get Root
    server.addTool(
        name = "get_root",
        description = "Get a greeting from the Cymbal Superstore Inventory API.",
        inputSchema = ToolSchema(properties = buildJsonObject { })
    ) {
        CallToolResult(content = listOf(TextContent(text = "🍎 Hello! This is the Cymbal Superstore Inventory API.")))
    }

    val transports = ConcurrentHashMap<String, SseServerTransport>()

    routing {
        // Status and Health
        get("/status") { 
            call.respond(mapOf("message" to "Cymbal Superstore Inventory API is running.", "db" to dbRunning))
        }
        get("/health") { 
            call.respond(mapOf("message" to "Healthy", "db" to dbRunning, "hostname" to (System.getenv("HOSTNAME") ?: "unknown")))
        }

        sse("/sse") { 
            val transport = SseServerTransport("/messages", this)
            transports[transport.sessionId] = transport
            
            logger.info("New session: ${transport.sessionId}")

            try {
                server.createSession(transport)
                // Wait until the SSE session is closed
                kotlinx.coroutines.awaitCancellation()
            } catch (e: Exception) {
                logger.error("Session error", e)
            } finally {
                transports.remove(transport.sessionId)
                logger.info("Session closed: ${transport.sessionId}")
            }
        }

        post("/messages") { 
            val sessionId = call.request.queryParameters["sessionId"]
            logger.info("POST /messages with sessionId: $sessionId")
            if (sessionId == null) {
                call.respond(HttpStatusCode.BadRequest, "Missing sessionId")
                return@post
            }
            val transport = transports[sessionId]
            if (transport == null) {
                logger.warn("No transport found for sessionId: $sessionId. Available: ${transports.keys}")
                call.respond(HttpStatusCode.BadRequest, "Invalid sessionId")
                return@post
            }

            transport.handlePostMessage(call)
        }
    }
}
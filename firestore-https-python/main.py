import logging
import sys
import os
import random
from datetime import datetime, timedelta, timezone
from typing import Dict, Any, List, Optional, Union

from starlette.responses import JSONResponse
from pythonjsonlogger.json import JsonFormatter
from fastmcp import FastMCP
from google.cloud import firestore  # type: ignore
from google.cloud.firestore import Client, DocumentSnapshot  # type: ignore
import dotenv

# Load environment variables
dotenv.load_dotenv()

# Set up logging
logger = logging.getLogger()
logger.setLevel(logging.INFO)

formatter = JsonFormatter()
stderr_handler = logging.StreamHandler(sys.stderr)
stderr_handler.setFormatter(formatter)
stderr_handler.setLevel(logging.INFO)
logger.addHandler(stderr_handler)

# Global Firestore client
db: Optional[Client] = None
db_running: bool = False


def init_firestore():
    global db, db_running
    try:
        db = firestore.Client()
        db_running = True
        logger.info("Firestore client initialized")
    except Exception as err:
        logger.error(
            "Failed to initialize Firestore", extra={"error": str(err)}
        )
        db_running = False


# Initialize FastMCP server
mcp = FastMCP("inventory-server")


# health check on standard http endpoint
@mcp.custom_route("/health", methods=["GET"])
async def health_check(request):
    return JSONResponse({"status": "healthy", "service": "firebase-mcp-server"})


# --- Helper Functions ---


def doc_to_product(doc: DocumentSnapshot) -> Dict[str, Any]:
    data = doc.to_dict()
    if not data:
        raise ValueError("Document data is empty")

    # Handle Firestore timestamps by converting them to strings or keeping
    # as datetime. For JSON serialization in MCP, strings might be safer,
    # but FastMCP handles basic types.
    # Let's match the TS output structure which likely serializes dates to
    # strings in JSON.

    timestamp = data.get("timestamp")
    actualdateadded = data.get("actualdateadded")

    return {
        "id": doc.id,
        "name": data.get("name"),
        "price": data.get("price"),
        "quantity": data.get("quantity"),
        "imgfile": data.get("imgfile"),
        "timestamp": timestamp.isoformat() if timestamp else None,
        "actualdateadded": (
            actualdateadded.isoformat() if actualdateadded else None
        ),
    }


def add_or_update_firestore(product: Dict[str, Any]):
    if not db:
        return

    collection_ref = db.collection("inventory")
    query = collection_ref.where(
        filter=firestore.FieldFilter("name", "==", product["name"])
    ).stream()

    docs = list(query)

    if not docs:
        collection_ref.add(product)
    else:
        for doc in docs:
            collection_ref.document(doc.id).update(product)


def init_firestore_collection():
    if not db:
        return

    old_products = [
        "Apples",
        "Bananas",
        "Milk",
        "Whole Wheat Bread",
        "Eggs",
        "Cheddar Cheese",
        "Whole Chicken",
        "Rice",
        "Black Beans",
        "Bottled Water",
        "Apple Juice",
        "Cola",
        "Coffee Beans",
        "Green Tea",
        "Watermelon",
        "Broccoli",
        "Jasmine Rice",
        "Yogurt",
        "Beef",
        "Shrimp",
        "Walnuts",
        "Sunflower Seeds",
        "Fresh Basil",
        "Cinnamon",
    ]

    for product_name in old_products:
        # random.random() * 10 + 1  => 1.0 to 11.0
        # Math.floor(...) => integer 1 to 10
        price = int(random.random() * 10) + 1
        quantity = int(random.random() * 500) + 1

        # Date logic:
        # TS: Date.now() - Math.floor(Math.random() * 31536000000) - 7776000000
        # 31536000000 ms is approx 1 year (365 days)
        # 7776000000 ms is approx 90 days

        random_ms = int(random.random() * 31536000000)
        offset_ms = 7776000000
        delta_ms = random_ms + offset_ms
        timestamp = datetime.now(timezone.utc) - timedelta(
            milliseconds=delta_ms
        )

        product = {
            "name": product_name,
            "price": price,
            "quantity": quantity,
            "imgfile": (
                f"product-images/{product_name.replace(' ', '').lower()}.png"
            ),
            "timestamp": timestamp,
            "actualdateadded": datetime.now(timezone.utc),
        }
        logger.info(
            f"⬆️ Adding (or updating) product in firestore: {product['name']}"
        )
        add_or_update_firestore(product)

    recent_products = [
        "Parmesan Crisps",
        "Pineapple Kombucha",
        "Maple Almond Butter",
        "Mint Chocolate Cookies",
        "White Chocolate Caramel Corn",
        "Acai Smoothie Packs",
        "Smores Cereal",
        "Peanut Butter and Jelly Cups",
    ]

    for product_name in recent_products:
        price = int(random.random() * 10) + 1
        quantity = int(random.random() * 100) + 1

        # TS: Date.now() - Math.floor(Math.random() * 518400000) + 1
        # 518400000 ms = 6 days
        random_ms = int(random.random() * 518400000)
        # The +1 is negligible for logic but implies strict past or 'now'

        timestamp = datetime.now(timezone.utc) - timedelta(
            milliseconds=random_ms
        )

        product = {
            "name": product_name,
            "price": price,
            "quantity": quantity,
            "imgfile": (
                f"product-images/{product_name.replace(' ', '').lower()}.png"
            ),
            "timestamp": timestamp,
            "actualdateadded": datetime.now(timezone.utc),
        }
        logger.info(
            f"🆕 Adding (or updating) product in firestore: {product['name']}"
        )
        add_or_update_firestore(product)

    recent_products_oos = ["Wasabi Party Mix", "Jalapeno Seasoning"]
    for product_name in recent_products_oos:
        price = int(random.random() * 10) + 1
        quantity = 0

        random_ms = int(random.random() * 518400000)
        timestamp = datetime.now(timezone.utc) - timedelta(
            milliseconds=random_ms
        )

        product = {
            "name": product_name,
            "price": price,
            "quantity": quantity,
            "imgfile": (
                f"product-images/{product_name.replace(' ', '').lower()}.png"
            ),
            "timestamp": timestamp,
            "actualdateadded": datetime.now(timezone.utc),
        }
        logger.info(
            f"😱 Adding (or updating) out of stock product in firestore: "
            f"{product['name']}"
        )
        add_or_update_firestore(product)


def clean_firestore_collection():
    if not db:
        return
    logger.info("Cleaning Firestore collection...")
    # List all documents in 'inventory' collection
    # Note: Stream is more memory efficient than get() for large collections
    docs = db.collection("inventory").stream()

    # Batch delete
    batch = db.batch()
    count = 0
    for doc in docs:
        batch.delete(doc.reference)
        count += 1
        if count >= 400:  # Firestore batch limit is 500
            batch.commit()
            batch = db.batch()
            count = 0
    if count > 0:
        batch.commit()

    logger.info("Firestore collection cleaned.")


# --- MCP Tools ---


@mcp.tool()
def get_products() -> Union[List[Dict[str, Any]], Dict[str, str]]:
    """
    Get a list of all products from the inventory database.
    """
    if not db_running or not db:
        return {"error": "Inventory database is not running."}

    products_ref = db.collection("inventory")
    docs = products_ref.stream()

    products_array = [doc_to_product(doc) for doc in docs]
    return products_array


@mcp.tool()
def get_product_by_id(id: str) -> Union[Dict[str, Any], Dict[str, str]]:
    """
    Get a single product from the inventory database by its ID.
    """
    if not db_running or not db:
        return {"error": "Inventory database is not running."}

    doc_ref = db.collection("inventory").document(id)
    doc = doc_ref.get()

    if not doc.exists:
        return {"error": "Product not found."}

    return doc_to_product(doc)


@mcp.tool()
def search_products_by_name(
    name: str,
) -> Union[List[Dict[str, Any]], Dict[str, str]]:
    """
    Search for products in the inventory database by name (prefix match).
    Case-sensitive.
    """
    if not db_running or not db:
        return {"error": "Inventory database is not running."}

    products_ref = db.collection("inventory")
    # Prefix search pattern
    query = (
        products_ref.where(filter=firestore.FieldFilter("name", ">=", name))
        .where(filter=firestore.FieldFilter("name", "<", name + "\uf8ff"))
        .stream()
    )

    products_array = [doc_to_product(doc) for doc in query]
    return products_array


@mcp.tool()
def seed() -> str:
    """
    Seed the inventory database with products.
    """
    if not db_running or not db:
        return "Inventory database is not running."

    init_firestore_collection()
    return "Database seeded successfully."


@mcp.tool()
def reset() -> str:
    """
    Clears all products from the inventory database.
    """
    if not db_running or not db:
        return "Inventory database is not running."

    clean_firestore_collection()
    return "Database reset successfully."


@mcp.tool()
def get_root() -> str:
    """
    Get a greeting from the Cymbal Superstore Inventory API.
    """
    return "🍎 Hello! This is the Cymbal Superstore Inventory API."


@mcp.tool()
def check_db() -> str:
    """
    Checks if the inventory database is running.
    """
    return f"Database running: {db_running}"


@mcp.tool()
def get_firestore_stats() -> Union[Dict[str, Any], Dict[str, str]]:
    """
    Report Firestore statistics and insights for the inventory collection.
    Provides document counts and performance metadata via query explanation.
    """
    if not db_running or not db:
        return {"error": "Inventory database is not running."}

    try:
        from google.cloud.firestore_v1.query_profile import ExplainOptions
        from dataclasses import asdict

        inventory_ref = db.collection("inventory")

        # 1. Document Count (Aggregation)
        # Efficiently get the total number of documents in a collection.
        count_query = inventory_ref.count()
        count_results = count_query.get()
        # AggregationQuery.get() returns a list of lists of AggregationResult
        total_documents = (
            count_results[0][0].value
            if count_results and count_results[0]
            else 0
        )

        # 2. Query Insight Sample
        # Explaining a query provides "advanced Query Insights for performance"
        # such as index usage and execution time.
        query = inventory_ref.where(
            filter=firestore.FieldFilter("quantity", ">", 0)
        )
        # In Python SDK, use get(explain_options=...) instead of explain()
        results = query.get(explain_options=ExplainOptions(analyze=True))
        explain_metrics = results.get_explain_metrics()

        # Convert metrics to serializable format
        plan_summary = asdict(explain_metrics.plan_summary)
        execution_stats = (
            asdict(explain_metrics.execution_stats)
            if explain_metrics.execution_stats
            else None
        )

        # Handle non-serializable objects like timedelta in execution_stats
        if execution_stats and "execution_duration" in execution_stats:
            execution_stats["execution_duration"] = str(
                execution_stats["execution_duration"]
            )

        insights = {
            "query_description": "Documents with quantity > 0",
            "plan_summary": plan_summary,
            "execution_stats": execution_stats,
        }

        return {
            "project_id": db.project,
            "database_id": db._database,
            "collection_stats": {
                "name": "inventory",
                "total_documents": total_documents,
            },
            "performance_insights": insights,
            "monitoring_links": {
                "usage": (
                    f"https://console.cloud.google.com/firestore/databases/"
                    f"{db._database}/usage?project={db.project}"
                ),
                "metrics": (
                    f"https://console.cloud.google.com/monitoring/dashboards/"
                    f"resourceList/firestore_instance?project={db.project}"
                ),
            },
            "note": (
                "For real-time usage monitoring (reads, writes, storage, "
                "latency), please refer to the Google Cloud Console links "
                "provided."
            ),
        }
    except Exception as e:
        logger.error(f"Error gathering Firestore stats: {e}")
        return {"error": f"Failed to gather stats: {str(e)}"}


if __name__ == "__main__":
    init_firestore()
    port = int(os.getenv("PORT", 8080))
    logger.info(f"🚀 MCP server started on port {port}")

    mcp.run(
        transport="http",
        host="0.0.0.0",
        port=port,
    )

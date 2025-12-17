import logging
import sys
import random
from datetime import datetime, timedelta, timezone
from typing import Dict, Any, List, Optional, Union

from pythonjsonlogger.json import JsonFormatter
from mcp.server.fastmcp import FastMCP
from google.cloud import firestore
from google.cloud.firestore import Client, DocumentSnapshot
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
        "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs",
        "Cheddar Cheese", "Whole Chicken", "Rice", "Black Beans",
        "Bottled Water", "Apple Juice", "Cola", "Coffee Beans",
        "Green Tea", "Watermelon", "Broccoli", "Jasmine Rice",
        "Yogurt", "Beef", "Shrimp", "Walnuts", "Sunflower Seeds",
        "Fresh Basil", "Cinnamon",
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
        timestamp = (
            datetime.now(timezone.utc) - timedelta(milliseconds=delta_ms)
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
        "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
        "Mint Chocolate Cookies", "White Chocolate Caramel Corn",
        "Acai Smoothie Packs", "Smores Cereal", "Peanut Butter and Jelly Cups",
    ]

    for product_name in recent_products:
        price = int(random.random() * 10) + 1
        quantity = int(random.random() * 100) + 1

        # TS: Date.now() - Math.floor(Math.random() * 518400000) + 1
        # 518400000 ms = 6 days
        random_ms = int(random.random() * 518400000)
        # The +1 is negligible for logic but implies strict past or 'now'

        timestamp = (
            datetime.now(timezone.utc) - timedelta(milliseconds=random_ms)
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
        timestamp = (
            datetime.now(timezone.utc) - timedelta(milliseconds=random_ms)
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


if __name__ == "__main__":
    init_firestore()
    mcp.run(transport="stdio")

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { Firestore, DocumentData, DocumentSnapshot } from "@google-cloud/firestore";
import logger from "./logger";
import dotenv from "dotenv";


// Init env variables (from .env or at runtime from Cloud Run)
dotenv.config();

let firestore: Firestore;
let dbRunning: boolean;

// Product object definition
interface Product {
  id?: string;
  name: string;
  price: number;
  quantity: number;
  imgfile: string;
  timestamp: Date;
  actualdateadded: Date;
}

// Helper function to convert a Firestore document to a Product object
function docToProduct(doc: DocumentSnapshot<DocumentData>): Product {
  const data = doc.data();
  if (!data) {
    throw new Error("Document data is empty");
  }
  return {
    id: doc.id,
    name: data.name,
    price: data.price,
    quantity: data.quantity,
    imgfile: data.imgfile,
    timestamp: data.timestamp,
    actualdateadded: data.actualdateadded,
  };
}

// ---------------- MCP SERVER ------------------------------------------------
const mcpServer = new McpServer({
  name: "inventory-server",
  version: "1.0.0",
});

mcpServer.registerTool(
  "get_products",
  {
    title: "Get all products",
    description: "Get a list of all products from the inventory database",
    inputSchema: {},
  },
  async () => {
    if (!dbRunning) {
      return { content: [{ type: "text", text: "Inventory database is not running." }], isError: true };
    }
    const products = await firestore.collection("inventory").get();
    const productsArray: Product[] = products.docs.map(docToProduct);
    return { content: [{ type: "text", text: JSON.stringify(productsArray, null, 2) }] };
  }
);

const getProductByIdSchema = {
  id: z.string().describe("The ID of the product to get"),
};

mcpServer.registerTool(
  "get_product_by_id",
  {
    title: "Get product by ID",
    description: "Get a single product from the inventory database by its ID",
    inputSchema: getProductByIdSchema as any,
  },
  async (args: any) => {
    const { id } = args;
    if (!dbRunning) {
      return { content: [{ type: "text" as const, text: "Inventory database is not running." }], isError: true };
    }
    const productDoc = await firestore.collection("inventory").doc(id).get();
    if (!productDoc.exists) {
      return { content: [{ type: "text" as const, text: "Product not found." }], isError: true };
    }
    const product = docToProduct(productDoc);
    return { content: [{ type: "text" as const, text: JSON.stringify(product, null, 2) }] };
  }
);

mcpServer.registerTool(
  "seed",
  {
    title: "Seed database",
    description: "Seed the inventory database with products.",
    inputSchema: {},
  },
  async () => {
    if (!dbRunning) {
      return { content: [{ type: "text", text: "Inventory database is not running." }], isError: true };
    }
    await initFirestoreCollection();
    return { content: [{ type: "text", text: "Database seeded successfully." }] };
  }
);

mcpServer.registerTool(
  "reset",
  {
    title: "Reset database",
    description: "Clears all products from the inventory database.",
    inputSchema: {},
  },
  async () => {
    if (!dbRunning) {
      return { content: [{ type: "text", text: "Inventory database is not running." }], isError: true };
    }
    await cleanFirestoreCollection();
    return { content: [{ type: "text", text: "Database reset successfully." }] };
  }
);


export const handleGetRoot = async () => {
  return { content: [{ type: "text" as const, text: "🍎 Hello! This is the Cymbal Superstore Inventory API." }] };
};

mcpServer.registerTool(
  "get_root",
  {
    title: "Get root",
    description: "Get a greeting from the Cymbal Superstore Inventory API.",
    inputSchema: {},
  },
  handleGetRoot
);


// ... (other code)

export const handleCheckDb = async () => {
  const isRunning = await checkdb();
  return { content: [{ type: "text" as const, text: `Database running: ${isRunning}` }] };
};

mcpServer.registerTool(
  "check_db",
  {
    title: "Check DB Status",
    description: "Checks if the inventory database is running.",
    inputSchema: {},
  },
  handleCheckDb
);


// This will overwrite products in the database - this is intentional, to keep the date-added fresh. (always have a list of products added < 1 week ago, so that
// the new products page always has items to show.
async function initFirestoreCollection() {
  const oldProducts = [
    "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese",
    "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice",
    "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli",
    "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts",
    "Sunflower Seeds", "Fresh Basil", "Cinnamon",
  ];

  for (const productName of oldProducts) {
    const oldProduct: Omit<Product, 'id'> = {
      name: productName,
      price: Math.floor(Math.random() * 10) + 1,
      quantity: Math.floor(Math.random() * 500) + 1,
      imgfile: `product-images/${productName.replace(/\s/g, "").toLowerCase()}.png`,
      timestamp: new Date(Date.now() - Math.floor(Math.random() * 31536000000) - 7776000000),
      actualdateadded: new Date(Date.now()),
    };
    logger.info(`⬆️ Adding (or updating) product in firestore: ${oldProduct.name}`);
    await addOrUpdateFirestore(oldProduct);
  }

  const recentProducts = [
    "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter",
    "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs",
    "Smores Cereal", "Peanut Butter and Jelly Cups",
  ];

  for (const productName of recentProducts) {
    const recent: Omit<Product, 'id'> = {
      name: productName,
      price: Math.floor(Math.random() * 10) + 1,
      quantity: Math.floor(Math.random() * 100) + 1,
      imgfile: `product-images/${productName.replace(/\s/g, "").toLowerCase()}.png`,
      timestamp: new Date(Date.now() - Math.floor(Math.random() * 518400000) + 1),
      actualdateadded: new Date(Date.now()),
    };
    logger.info(`🆕 Adding (or updating) product in firestore: ${recent.name}`);
    await addOrUpdateFirestore(recent);
  }

  const recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
  for (const productName of recentProductsOutOfStock) {
    const oosProduct: Omit<Product, 'id'> = {
      name: productName,
      price: Math.floor(Math.random() * 10) + 1,
      quantity: 0,
      imgfile: `product-images/${productName.replace(/\s/g, "").toLowerCase()}.png`,
      timestamp: new Date(Date.now() - Math.floor(Math.random() * 518400000) + 1),
      actualdateadded: new Date(Date.now()),
    };
    logger.info(`😱 Adding (or updating) out of stock product in firestore: ${oosProduct.name}`);
    await addOrUpdateFirestore(oosProduct);
  }
}

// Helper - add Firestore doc if not exists, otherwise update
async function addOrUpdateFirestore(product: Omit<Product, 'id'>) {
  const querySnapshot = await firestore
    .collection("inventory")
    .where("name", "==", product.name)
    .get();

  if (querySnapshot.empty) {
    await firestore.collection("inventory").add(product);
  } else {
    for (const doc of querySnapshot.docs) {
      await firestore.collection("inventory").doc(doc.id).update(product);
    }
  }
}

async function cleanFirestoreCollection() {
  logger.info("Cleaning Firestore collection...");
  const snapshot = await firestore.collection("inventory").get();
  if (!snapshot.empty) {
    const batch = firestore.batch();
    snapshot.docs.forEach((doc) => {
      batch.delete(doc.ref);
    });
    await batch.commit();
  }
  logger.info("Firestore collection cleaned.");
}

export { mcpServer };

async function checkdb(): Promise<boolean> {
  return dbRunning;
}

async function main() {
  try {
    firestore = new Firestore();
    dbRunning = true;
    logger.info("Firestore client initialized");
  } catch (err) {
    logger.error("Failed to initialize Firestore", err);
    dbRunning = false;
  }

  const transport = new StdioServerTransport();
  await mcpServer.connect(transport);
  logger.info("MCP Server connected via Stdio");
}

if (require.main === module) {
  main().catch((error) => {
    logger.error("Server error:", error);
    process.exit(1);
  });
}


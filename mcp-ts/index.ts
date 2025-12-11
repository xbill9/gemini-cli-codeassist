// # Copyright 2023 Google LLC
// #
// # Licensed under the Apache License, Version 2.0 (the "License");
// # you may not use this file except in compliance with the License.
// # You may obtain a copy of the License at
// #
// #      http://www.apache.org/licenses/LICENSE-2.0
// #
// # Unless required by applicable law or agreed to in writing, software
// # distributed under the License is distributed on an "AS IS" BASIS,
// # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// # See the License for the specific language governing permissions and
// # limitations under the License.

// Init Express.js
import express, { Express, Request, Response } from "express";
import { randomUUID } from "node:crypto";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import { isInitializeRequest } from "@modelcontextprotocol/sdk/types.js";
import { z } from "zod";
import cors from "cors";
import { Firestore, DocumentData, DocumentSnapshot } from "@google-cloud/firestore";
import logger from "./logger";

const app: Express = express();
app.use(express.json());
app.use(cors({
  origin: '*',
  exposedHeaders: ['Mcp-Session-Id'],
  allowedHeaders: ['Content-Type', 'mcp-session-id'],
}));

export default app;

// Init env variables (from .env or at runtime from Cloud Run)
import dotenv from "dotenv";
dotenv.config();
const port = process.env.PORT || 3000;

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

const GetProductByIdInputSchema = z.object({
  id: z.string().describe("The ID of the product to get"),
});

mcpServer.registerTool(
  "get_product_by_id",
  {
    title: "Get product by ID",
    description: "Get a single product from the inventory database by its ID",
    inputSchema: GetProductByIdInputSchema,
  },
  async (args: { id: string }) => {
    const { id } = args;
    if (!dbRunning) {
      return { content: [{ type: "text", text: "Inventory database is not running." }], isError: true };
    }
    const productDoc = await firestore.collection("inventory").doc(id).get();
    if (!productDoc.exists) {
      return { content: [{ type: "text", text: "Product not found." }], isError: true };
    }
    const product = docToProduct(productDoc);
    return { content: [{ type: "text", text: JSON.stringify(product, null, 2) }] };
  }
);
mcpServer.registerTool(
  "seed_database",
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
  "get_root",
  {
    title: "Get root",
    description: "Get a greeting from the Cymbal Superstore Inventory API.",
    inputSchema: {},
  },
  async () => {
    return { content: [{ type: "text", text: "🍎 Hello! This is the Cymbal Superstore Inventory API." }] };
  }
);

const transports: { [sessionId: string]: StreamableHTTPServerTransport } = {};

app.post('/mcp', async (req, res) => {
  const sessionId = req.headers['mcp-session-id'] as string | undefined;
  let transport: StreamableHTTPServerTransport;

  if (sessionId && transports[sessionId]) {
    transport = transports[sessionId];
  } else if (!sessionId && isInitializeRequest(req.body)) {
    transport = new StreamableHTTPServerTransport({
      sessionIdGenerator: () => randomUUID(),
      onsessioninitialized: (sessionId) => {
        transports[sessionId] = transport;
      },
    });
    transport.onclose = () => {
      if (transport.sessionId) {
        delete transports[transport.sessionId];
      }
    };
    await mcpServer.connect(transport);
  } else {
    res.status(400).json({
      jsonrpc: '2.0',
      error: { code: -32000, message: 'Bad Request: No valid session ID provided' },
      id: null,
    });
    return;
  }
  await transport.handleRequest(req, res, req.body);
});

const handleSessionRequest = async (req: express.Request, res: express.Response) => {
  const sessionId = req.headers['mcp-session-id'] as string | undefined;
  if (!sessionId || !transports[sessionId]) {
    res.status(400).send('Invalid or missing session ID');
    return;
  }
  const transport = transports[sessionId];
  await transport.handleRequest(req, res);
};

app.get('/mcp', handleSessionRequest);
app.delete('/mcp', handleSessionRequest);


// ---------------- HANDLERS ------------------------------------------------
app.get("/", (req: Request, res: Response) => {
  res.send("🍎 Hello! This is the Cymbal Superstore Inventory API.");
});

app.get("/health", (req: Request, res: Response) => {
  res.send("✅ ok");
});

// Get all products from the database
app.get("/products", async (req: Request, res: Response) => {
  if (!dbRunning) {
    res.status(500).send("Inventory database is not running.");
    return;
  }

  const products = await firestore.collection("inventory").get();
  const productsArray: Product[] = products.docs.map(docToProduct);
  res.send(productsArray);
});

// Get product by ID
app.get("/products/:id", async (req: Request, res: Response) => {
  if (!dbRunning) {
    res.status(500).send("Inventory database is not running.");
    return;
  }

  const q_id = req.params.id;
  const productDoc = await firestore.collection("inventory").doc(q_id).get();
  if (!productDoc.exists) {
    res.status(404).send("Product not found.");
    return;
  }

  const product = docToProduct(productDoc);
  res.send(product);
});

app.get("/seed", async (req: Request, res: Response) => {
  if (!dbRunning) {
    res.status(500).send("Inventory database is not running.");
    return;
  }
  await initFirestoreCollection();
  res.send("Database seeded successfully.");
});

// ------------------- ------------------- ------------------- ------------------- -------------------
// START EXPRESS SERVER
// ------------------- ------------------- ------------------- ------------------- -------------------
async function startServer() {
  try {
    firestore = new Firestore();
    // Perform a simple read to check the connection
    await firestore.collection('inventory').limit(1).get();
    dbRunning = true;
  } catch (e) {
    logger.error("Error connecting to Firestore:", e);
    dbRunning = false;
  }

  if (process.env.NODE_ENV !== "test") {
    app.listen(port, () => {
      logger.info(`🍏 Cymbal Superstore: Inventory API running on port: ${port} DB Running (${dbRunning})`);
    });
  }
}

startServer();

// ------------------- ------------------- ------------------- ------------------- -------------------
// HELPERS -- SEED THE INVENTORY DATABASE (PRODUCTS)
// ------------------- ------------------- ------------------- ------------------- -------------------

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
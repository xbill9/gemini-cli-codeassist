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

import express, { Request, Response } from "express";
import { randomUUID } from "node:crypto";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import { isInitializeRequest } from "@modelcontextprotocol/sdk/types.js";
import { z } from "zod";
import cors from "cors";
import { Firestore, DocumentData, DocumentSnapshot } from "@google-cloud/firestore";
import dotenv from "dotenv";
import logger from "./logger.js";

dotenv.config();

// Initialize Express App
const app = express();
app.use(express.json());
app.use(cors({
  origin: '*',
  exposedHeaders: ['Mcp-Session-Id'],
  allowedHeaders: ['Content-Type', 'mcp-session-id'],
}));

// Validate essential environment variables
function validateEnvVars() {
  const requiredEnvVars = ["GOOGLE_CLOUD_PROJECT"];
  for (const envVar of requiredEnvVars) {
    if (!process.env[envVar]) {
      // Warn but don't crash if running locally without GCP context, 
      // though Firestore might fail later if no creds are present.
      logger.warn(`Missing environment variable: ${envVar}`);
    }
  }
}

// ---------------- FIRESTORE SETUP -------------------------------------------
let firestore: Firestore;
let dbRunning: boolean = false;

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
    timestamp: data.timestamp ? data.timestamp.toDate() : new Date(), // Handle Firestore Timestamp
    actualdateadded: data.actualdateadded ? data.actualdateadded.toDate() : new Date(),
  };
}

// ---------------- MCP SERVER ------------------------------------------------
const mcpServer = new McpServer({
  name: "cymbal-inventory",
  version: "1.0.0",
});

// Register existing Greeting Tool
mcpServer.registerTool(
    'greet',
    {
        title: 'Greeting Tool',
        description: 'A simple greeting tool',
        inputSchema: z.object({
            name: z.string().describe('Name to greet')
        }) as any
    },
    async (args: any) => {
        const { name } = args as { name: string };
        return {
            content: [
                {
                    type: 'text' as const,
                    text: `Hello, ${name}!`
                }
            ]
        };
    }
);

// Register Inventory Tools
mcpServer.registerTool(
  "get_products",
  {
    title: "Get all products",
    description: "Get a list of all products from the inventory database",
    inputSchema: z.object({}) as any,
  },
  async () => {
    if (!dbRunning) {
      return { content: [{ type: "text" as const, text: "Inventory database is not running." }], isError: true };
    }
    try {
        const products = await firestore.collection("inventory").get();
        const productsArray: Product[] = products.docs.map(docToProduct);
        return { content: [{ type: "text" as const, text: JSON.stringify(productsArray, null, 2) }] };
    } catch (e) {
        logger.error("Error fetching products:", e);
        return { content: [{ type: "text" as const, text: "Error fetching products." }], isError: true };
    }
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
    inputSchema: GetProductByIdInputSchema as any,
  },
  async (args: any) => {
    const { id } = args as { id: string };
    if (!dbRunning) {
      return { content: [{ type: "text" as const, text: "Inventory database is not running." }], isError: true };
    }
    try {
        const productDoc = await firestore.collection("inventory").doc(id).get();
        if (!productDoc.exists) {
        return { content: [{ type: "text" as const, text: "Product not found." }], isError: true };
        }
        const product = docToProduct(productDoc);
        return { content: [{ type: "text" as const, text: JSON.stringify(product, null, 2) }] };
    } catch (e) {
        logger.error(`Error fetching product ${id}:`, e);
        return { content: [{ type: "text" as const, text: "Error fetching product." }], isError: true };
    }
  }
);

mcpServer.registerTool(
  "seed",
  {
    title: "Seed database",
    description: "Seed the inventory database with products.",
    inputSchema: z.object({}) as any,
  },
  async () => {
    if (!dbRunning) {
      return { content: [{ type: "text" as const, text: "Inventory database is not running." }], isError: true };
    }
    await initFirestoreCollection();
    return { content: [{ type: "text" as const, text: "Database seeded successfully." }] };
  }
);

mcpServer.registerTool(
  "reset",
  {
    title: "Reset database",
    description: "Clear all products from the inventory database.",
    inputSchema: z.object({}) as any,
  },
  async () => {
    if (!dbRunning) {
      return { content: [{ type: "text" as const, text: "Inventory database is not running." }], isError: true };
    }
    await cleanFirestoreCollection();
    return { content: [{ type: "text" as const, text: "Database reset successfully." }] };
  }
);

mcpServer.registerTool(
  "get_root",
  {
    title: "Get root",
    description: "Get a greeting from the Cymbal Superstore Inventory API.",
    inputSchema: z.object({}) as any,
  },
  async () => {
    return { content: [{ type: "text" as const, text: "🍎 Hello! This is the Cymbal Superstore Inventory API." }] };
  }
);

// ---------------- MCP TRANSPORT ---------------------------------------------
const transports: { [sessionId: string]: StreamableHTTPServerTransport } = {};

app.post('/mcp', async (req: Request, res: Response) => {
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

const handleSessionRequest = async (req: Request, res: Response) => {
  const sessionId = req.headers['mcp-session-id'] as string | undefined;
  if (!sessionId || !transports[sessionId]) {
    res.status(400).send('Invalid or missing session ID');
    return;
  }
  const transport = transports[sessionId];
  if (req.method === 'DELETE') {
    if (transport.sessionId) {
      delete transports[transport.sessionId];
      logger.info(`MCP session ${transport.sessionId} explicitly terminated via DELETE request.`);
      res.status(204).send(); // 204 No Content for successful deletion
      return;
    }
  }
  await transport.handleRequest(req, res);
};

app.get('/mcp', handleSessionRequest);
app.delete('/mcp', handleSessionRequest);


// ---------------- REST HANDLERS ---------------------------------------------
app.get("/status", (req: Request, res: Response) => {
  res.status(200).json({ message: "Cymbal Superstore Inventory API is running.", db: dbRunning });
});

app.get("/health", (req: Request, res: Response) => {
  res.status(200).json({ message: "Healthy", db: dbRunning });
});


// ---------------- DATABASE HELPERS ------------------------------------------

async function cleanFirestoreCollection() {
  const collectionRef = firestore.collection('inventory');
  const snapshot = await collectionRef.get();
  
  if (snapshot.empty) {
    return;
  }

  // Firestore batches are limited to 500 operations
  const MAX_BATCH_SIZE = 500;
  const docs = snapshot.docs;
  
  for (let i = 0; i < docs.length; i += MAX_BATCH_SIZE) {
    const batch = firestore.batch();
    const chunk = docs.slice(i, i + MAX_BATCH_SIZE);
    chunk.forEach(doc => batch.delete(doc.ref));
    await batch.commit();
  }
}

// This will overwrite products in the database
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


// ---------------- SERVER STARTUP --------------------------------------------
const PORT = process.env.PORT || 8080;

const startServer = async () => {
  // Try initializing Firestore
  try {
      validateEnvVars();
      firestore = new Firestore();
      // Perform a simple read to check the connection
      await firestore.collection('inventory').limit(1).get();
      dbRunning = true;
      logger.info("Firestore connected successfully.");
  } catch (e) {
      logger.error("Error connecting to Firestore:", e);
      dbRunning = false;
  }

  const serverInstance = app.listen(PORT, () => {
    logger.info(`🍏 Cymbal Superstore: Inventory API & MCP Server running on port: ${PORT} DB Running (${dbRunning})`);
  });
  return serverInstance;
};

// Start the server if this file is run directly
if (require.main === module) {
  startServer().catch(error => {
    logger.error(`Failed to start server: ${(error as Error).message}`);
    process.exit(1);
  });
}

// Handle server shutdown
process.on('SIGINT', async () => {
    logger.info('Shutting down server...');
    process.exit(0);
});

export { app, startServer };

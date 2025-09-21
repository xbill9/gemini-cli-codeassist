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

var firestore;
var dbRunning;

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
    const productsArray: any[] = [];
    products.forEach((product) => {
      const p: Product = {
        id: product.id,
        name: product.data().name,
        price: product.data().price,
        quantity: product.data().quantity,
        imgfile: product.data().imgfile,
        timestamp: product.data().timestamp,
        actualdateadded: product.data().actualdateadded,
      };
      productsArray.push(p);
    });
    return { content: [{ type: "text", text: JSON.stringify(productsArray, null, 2) }] };
  }
);

mcpServer.registerTool(
  "get_product_by_id",
  {
    title: "Get product by ID",
    description: "Get a single product from the inventory database by its ID",
    inputSchema: {
      id: z.string().describe("The ID of the product to get"),
    },
  },
  async ({ id }) => {
    if (!dbRunning) {
      return { content: [{ type: "text", text: "Inventory database is not running." }], isError: true };
    }
    const product = await firestore.collection("inventory").doc(id).get();
    if (!product.exists) {
      return { content: [{ type: "text", text: "Product not found." }], isError: true };
    }
    const p: Product = {
      id: product.id,
      name: product.data().name,
      price: product.data().price,
      quantity: product.data().quantity,
      imgfile: product.data().imgfile,
      timestamp: product.data().timestamp,
      actualdateadded: product.data().actualdateadded,
    };
    return { content: [{ type: "text", text: JSON.stringify(p, null, 2) }] };
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
  const productsArray: any[] = [];
  products.forEach((product) => {
    const p: Product = {
      id: product.id,
      name: product.data().name,
      price: product.data().price,
      quantity: product.data().quantity,
      imgfile: product.data().imgfile,
      timestamp: product.data().timestamp,
      actualdateadded: product.data().actualdateadded,
    };
    productsArray.push(p);
  });
  res.send(productsArray);
});

// Get product by ID
app.get("/products/:id", async (req: Request, res: Response) => {
  if (!dbRunning) {
    res.status(500).send("Inventory database is not running.");
    return;
  }

  const q_id = req.params.id;
  const product = await firestore.collection("inventory").doc(q_id).get();
  if (!product.exists) {
    res.status(404).send("Product not found.");
    return;
  }

  const p: Product = {
    id: product.id,
    name: product.data().name,
    price: product.data().price,
    quantity: product.data().quantity,
    imgfile: product.data().imgfile,
    timestamp: product.data().timestamp,
    actualdateadded: product.data().actualdateadded,
  };
  res.send(p);
});

// ------------------- ------------------- ------------------- ------------------- -------------------
// START EXPRESS SERVER
// ------------------- ------------------- ------------------- ------------------- -------------------
  // Init Firestore client with product inventory
  const { Firestore } = require("@google-cloud/firestore");

  try {
    firestore = new Firestore();
    const collection = firestore.collection("inventory");
    const exists =  collection.get();

    dbRunning = !exists.empty
    } catch (e) {
      dbRunning = false;
    }

  if (dbRunning){
    initFirestoreCollection();
  }
  var server;
  if (process.env.NODE_ENV !== "test") {
    server = app.listen(port, () => {
      console.log(`🍏 Cymbal Superstore: Inventory API running on port: ${port} DB Running (${dbRunning})`);
    });
  }

// ------------------- ------------------- ------------------- ------------------- -------------------
// HELPERS -- SEED THE INVENTORY DATABASE (PRODUCTS)
// ------------------- ------------------- ------------------- ------------------- -------------------

// This will overwrite products in the database - this is intentional, to keep the date-added fresh. (always have a list of products added < 1 week ago, so that
// the new products page always has items to show.
function initFirestoreCollection() {
  const oldProducts = [
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
  ];
  // iterate over product names
  // add "old" products to firestore - all added between 1 month and 12 months ago
  // (none of these should show up in the new products list.)
  for (let i = 0; i < oldProducts.length; i++) {
    const oldProduct = {
      name: oldProducts[i],
      price: Math.floor(Math.random() * 10) + 1,
      quantity: Math.floor(Math.random() * 500) + 1,
      imgfile:
        "product-images/" +
        oldProducts[i].replace(/\s/g, "").toLowerCase() +
        ".png",
      // generate a random timestamp at least 3 months ago (but not more than 12 months ago)
      timestamp: new Date(
        Date.now() - Math.floor(Math.random() * 31536000000) - 7776000000
      ),

      actualdateadded: new Date(Date.now()),
    };
    console.log(
      "⬆️ Adding (or updating) product in firestore: " + oldProduct.name
    );
    addOrUpdateFirestore(oldProduct);
  }
  // Add recent products (force add last 7 days)
  const recentProducts = [
    "Parmesan Crisps",
    "Pineapple Kombucha",
    "Maple Almond Butter",
    "Mint Chocolate Cookies",
    "White Chocolate Caramel Corn",
    "Acai Smoothie Packs",
    "Smores Cereal",
    "Peanut Butter and Jelly Cups",
  ];
  for (let j = 0; j < recentProducts.length; j++) {
    const recent = {
      name: recentProducts[j],
      price: Math.floor(Math.random() * 10) + 1,
      quantity: Math.floor(Math.random() * 100) + 1,
      imgfile:
        "product-images/" +
        recentProducts[j].replace(/\s/g, "").toLowerCase() +
        ".png",
      timestamp: new Date(
        Date.now() - Math.floor(Math.random() * 518400000) + 1
      ),
      actualdateadded: new Date(Date.now()),
    };
    console.log("🆕 Adding (or updating) product in firestore: " + recent.name);
    addOrUpdateFirestore(recent);
  }

  // add recent products that are out of stock (To test demo query- only want to show in stock items.)
  const recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"];
  for (let k = 0; k < recentProductsOutOfStock.length; k++) {
    const oosProduct = {
      name: recentProductsOutOfStock[k],
      price: Math.floor(Math.random() * 10) + 1,
      quantity: 0,
      imgfile:
        "product-images/" +
        recentProductsOutOfStock[k].replace(/\s/g, "").toLowerCase() +
        ".png",
      timestamp: new Date(
        Date.now() - Math.floor(Math.random() * 518400000) + 1
      ),
      actualdateadded: new Date(Date.now()),
    };
    console.log(
      "😱 Adding (or updating) out of stock product in firestore: " +
        oosProduct.name
    );
    addOrUpdateFirestore(oosProduct);
  }
}

// Helper - add Firestore doc if not exists, otherwise update
// pass in a Product as the parameter
function addOrUpdateFirestore(product: Product) {
  firestore
    .collection("inventory")
    .where("name", "==", product.name)
    .get()
    .then((querySnapshot) => {
      if (querySnapshot.empty) {
        firestore.collection("inventory").add(product);
      } else {
        querySnapshot.forEach((doc) => {
          firestore.collection("inventory").doc(doc.id).update(product);
        });
      }
    });
}
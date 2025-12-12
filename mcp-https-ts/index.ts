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

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import { Request, Response } from "express";
import dotenv from "dotenv";
import * as z from 'zod/v3';
import { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { createMcpExpressApp } from "@modelcontextprotocol/sdk/server/express.js";

import logger from "./logger.js";

dotenv.config();

// Validate essential environment variables
function validateEnvVars() {
  const requiredEnvVars = ["PROJECT_ID"]; // Add other critical env vars if identified later

  for (const envVar of requiredEnvVars) {
    if (!process.env[envVar]) {
      throw new Error(`Missing required environment variable: ${envVar}`);
    }
  }
  logger.info("All essential environment variables are set.");
}

// validateEnvVars();
// ---------------- MCP SERVER ------------------------------------------------
const getServer = () => {
  const mcpServer = new McpServer({
    name: "hello-https-node",
    version: "1.0.0",
  });

  // Register a simple tool that returns a greeting
  mcpServer.registerTool(
      'greet',
      {
          title: 'Greeting Tool', // Display name for UI
          description: 'A simple greeting tool',
          inputSchema: {
              name: z.string().describe('Name to greet')
          }
      },
      async ({ name }): Promise<CallToolResult> => {
          return {
              content: [
                  {
                      type: 'text',
                      text: `Hello, ${name}!`
                  }
              ]
          };
      }
  );
  return mcpServer;
};

// Create and configure the Express app


const app = createMcpExpressApp();



app.post('/mcp', async (req: Request, res: Response) => {
    const mcpServer = getServer(); // Use the newly defined getServer function
    try {
        const transport: StreamableHTTPServerTransport = new StreamableHTTPServerTransport({
            sessionIdGenerator: undefined
        });
        await mcpServer.connect(transport);
        await transport.handleRequest(req, res, req.body);
        res.on('close', () => {
            logger.info('Request closed');
            transport.close();
            mcpServer.close();
        });
    } catch (error) {
                    logger.error('Error handling MCP request:', error);        if (!res.headersSent) {
            res.status(500).json({
                jsonrpc: '2.0',
                error: {
                    code: -32603,
                    message: 'Internal server error'
                },
                id: null
            });
        }
    }
});

app.get('/mcp', async (req: Request, res: Response) => {
    logger.info('Received GET MCP request');
    res.writeHead(405).end(
        JSON.stringify({
            jsonrpc: '2.0',
            error: {
                code: -32000,
                message: 'Method not allowed.'
            },
            id: null
        })
    );
});

app.delete('/mcp', async (req: Request, res: Response) => {
    logger.info('Received DELETE MCP request');
    res.writeHead(405).end(
        JSON.stringify({
            jsonrpc: '2.0',
            error: {
                code: -32000,
                message: 'Method not allowed.'
            },
            id: null
        })
    );
});

app.get("/health", (req: Request, res: Response) => {
  logger.info("Received GET request to /health");
  res.status(200).json({ message: "Healthy" });
});

// Start the server
const PORT = process.env.PORT || 8000;

const startServer = () => {
  validateEnvVars(); // Validate env vars before starting the server

  const serverInstance = app.listen(PORT, (error?: Error) => {
    if (error) {
      logger.error('Failed to start server:', error);
      process.exit(1);
    }
    logger.info(`MCP Stateless Streamable HTTP Server listening on port ${PORT}`);
  });
  return serverInstance;
};

// Start the server if this file is run directly
if (require.main === module) {
  try {
    validateEnvVars(); // Validate env vars before starting the server
    startServer();
  } catch (error) {
    logger.error(`Failed to start server: ${(error as Error).message}`);
    process.exit(1);
  }
}

// Handle server shutdown
process.on('SIGINT', async () => {
    logger.info('Shutting down server...');
    process.exit(0);
});

export { app, startServer };



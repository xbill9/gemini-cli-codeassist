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
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import dotenv from "dotenv";

import logger from "./logger.js";

dotenv.config();

// ---------------- MCP SERVER ------------------------------------------------
const mcpServer = new McpServer({
  name: "hello-world-server",
  version: "1.0.0",
});

mcpServer.registerTool(
  "greet",
  {
    title: "Minimal MCP over Node/TypeScript/Javascript",
    description: "Get a greeting from a local stdio server.",
    inputSchema: {},
  },
  greetHandler
);

export async function greetHandler() {
  logger.info("Executed greet tool");
  return { content: [{ type: "text" as const, text: "🍎 Hello! (MCP Enabled)." }] };
}

async function main() {
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

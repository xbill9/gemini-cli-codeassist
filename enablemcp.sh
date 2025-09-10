#!/bin/bash
source ~/gemini-cli-codeassist/set_env.sh
echo "Enabling Pre-Built MCP Support "

cp index.ts.mcp cymbal-superstore/backend/index.ts
cp package.json.mcp cymbal-superstore/backend/package.json

cd cymbal-superstore/backend
npm init





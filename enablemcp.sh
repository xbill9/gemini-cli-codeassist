#!/bin/bash
source ~/gemini-cli-codeassist/set_env.sh

echo "Cleaning existing build"
rm -rf cymbal-superstore

echo "Fetching Standard Cymbal SuperStore"
gsutil -m cp -rp gs://duet-appdev/cymbal-superstore .

echo "Enabling Pre-Built MCP Support "

cp index.ts.mcp cymbal-superstore/backend/index.ts
cp package.json.mcp cymbal-superstore/backend/package.json





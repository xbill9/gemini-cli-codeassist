{-# LANGUAGE OverloadedStrings #-}

module Main where

import MCP.Server
import Logic (myToolHandlers, logInfo)

-- Empty prompt handlers
promptHandlers :: (IO [PromptDefinition], PromptGetHandler IO)
promptHandlers = (pure [], \_ _ -> pure $ Left $ InvalidRequest "No prompts available")

-- Empty resource handlers
resourceHandlers :: (IO [ResourceDefinition], ResourceReadHandler IO)
resourceHandlers = (pure [], \_ -> pure $ Left $ ResourceNotFound "No resources available")

main :: IO ()
main = do
  logInfo "Starting Haskell MCP Server (HTTP)..."
  runMcpServerHttpWithConfig config serverInfo handlers
  where
    config = defaultHttpConfig
      { httpPort = 8080
      , httpHost = "0.0.0.0"
      , httpVerbose = True
      }
    serverInfo = McpServerInfo
      { serverName = "mcp-https-haskell"
      , serverVersion = "0.1.0"
      , serverInstructions = "A simple Haskell MCP server"
      }
    handlers = McpServerHandlers
      { prompts = Just promptHandlers
      , resources = Just resourceHandlers
      , tools = Just myToolHandlers
      }

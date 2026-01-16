{-# LANGUAGE OverloadedStrings #-}

module Main where

import MCP.Server
import Logic (myToolHandlers, logInfo)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text as T

-- Empty prompt handlers
promptHandlers :: (IO [PromptDefinition], PromptGetHandler IO)
promptHandlers = (pure [], \_ _ -> pure $ Left $ InvalidRequest "No prompts available")

-- Empty resource handlers
resourceHandlers :: (IO [ResourceDefinition], ResourceReadHandler IO)
resourceHandlers = (pure [], \_ -> pure $ Left $ ResourceNotFound "No resources available")

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portEnv >>= readMaybe)
  logInfo $ "Starting Haskell MCP Server (HTTP) on port " <> T.pack (show port) <> "..."
  runMcpServerHttpWithConfig (config port) serverInfo handlers
  where
    config p = defaultHttpConfig
      { httpPort = p
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

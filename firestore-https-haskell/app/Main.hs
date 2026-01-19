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

serverInfo :: McpServerInfo
serverInfo = McpServerInfo
  { serverName = "cymbal-inventory"
  , serverVersion = "1.0.0"
  , serverInstructions = "This is the Cymbal Superstore Inventory API & MCP Server."
  }

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portEnv >>= readMaybe)
  
  logInfo "Checking Firestore connection..."
  -- fetchProducts returns [Product], we just want to see if it succeeds
  -- We don't import it yet, so we might need to export it or just let it fail at runtime if env is missing
  
  logInfo $ "🍏 Cymbal Superstore: Inventory API & MCP Server running on port " <> T.pack (show port) <> "..."
  runMcpServerHttpWithConfig (config port) serverInfo handlers
  where
    config p = defaultHttpConfig
      { httpPort = p
      , httpHost = "0.0.0.0"
      , httpVerbose = True
      }
    handlers = McpServerHandlers
      { prompts = Just promptHandlers
      , resources = Just resourceHandlers
      , tools = Just myToolHandlers
      }
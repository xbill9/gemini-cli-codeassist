{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Logic
  ( handleMyTool
  , myToolHandlers
  , logInfo
  ) where

import MCP.Server
import MCP.Server.Derive (deriveToolHandlerWithDescription)
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO (stderr)
import Data.Time (getCurrentTime)
import Types

import qualified Data.Text as T
import Text.Read (readMaybe)

-- Helper for JSON logging to stderr
logJson :: Text -> Text -> IO ()
logJson level msg = do
  now <- getCurrentTime
  let logEntry = object
        [ "timestamp" .= now
        , "level" .= level
        , "message" .= msg
        ]
  BSL.hPutStrLn stderr (encode logEntry)

logInfo :: Text -> IO ()
logInfo = logJson "INFO"

-- This empty splice is required to separate the data type definition from the
-- Template Haskell splice that follows.
$(return [])

-- 2. Define a handler function
handleMyTool :: MyTool -> IO Content
handleMyTool (Greet n) = do
  logInfo $ "Greeting " <> n
  pure $ ContentText $ "Hello, " <> n <> "!"
handleMyTool (Sum v) = do
  logInfo $ "Summing values: " <> v
  let parts = T.splitOn "," v
  let maybeNums = map (readMaybe . T.unpack . T.strip) parts :: [Maybe Int]
  case sequence maybeNums of
    Just nums -> pure $ ContentText $ T.pack (show (sum nums))
    Nothing   -> pure $ ContentText "Error: Invalid input. Please provide a comma-separated list of integers."

-- 3. Use deriveToolHandlerWithDescription to generate the tool handlers.
myToolHandlers :: (IO [ToolDefinition], ToolCallHandler IO)
myToolHandlers = $(deriveToolHandlerWithDescription ''MyTool 'handleMyTool myToolDescriptions)

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Stdio
  ( -- * STDIO Transport
    transportRunStdio
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Time              (getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           System.IO              (hFlush, hPutStrLn, stderr, stdout)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Types


-- | Transport-specific implementation for STDIO
import           System.IO              (hSetEncoding, utf8)

-- | Log a message to stderr in JSON format
logJsonStdio :: T.Text -> IO ()
logJsonStdio msg = do
  now <- getCurrentTime
  let logEntry = object
        [ "timestamp" .= now
        , "level" .= ("INFO" :: T.Text)
        , "message" .= msg
        ]
  BSL.hPutStrLn stderr (encode logEntry)

transportRunStdio :: (MonadIO m) => McpServerInfo -> McpServerHandlers m -> m ()
transportRunStdio serverInfo handlers = do
  -- Ensure UTF-8 encoding for all handles
  liftIO $ do
    hSetEncoding stderr utf8
    hSetEncoding stdout utf8
  loop
  where
    loop = do
      input <- liftIO TIO.getLine
      when (not $ T.null $ T.strip input) $ do
        liftIO $ logJsonStdio $ "Received request: " <> input
        case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 input) of
          Left err -> liftIO $ logJsonStdio $ "Parse error: " <> T.pack err
          Right jsonValue -> do
            case parseJsonRpcMessage jsonValue of
              Left err -> liftIO $ logJsonStdio $ "JSON-RPC parse error: " <> T.pack err
              Right message -> do
                liftIO $ logJsonStdio $ "Processing message: " <> T.pack (show (getMessageSummary message))
                response <- handleMcpMessage serverInfo handlers message
                case response of
                  Just responseMsg -> do
                    liftIO $ logJsonStdio $ "Sending response for: " <> T.pack (show (getMessageSummary message))
                    let responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ encodeJsonRpcMessage responseMsg
                    liftIO $ TIO.putStrLn responseText
                    liftIO $ hFlush stdout
                  Nothing -> liftIO $ logJsonStdio $ "No response needed for: " <> T.pack (show (getMessageSummary message))
        loop


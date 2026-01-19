{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic where

import MCP.Server
import MCP.Server.Derive (deriveToolHandlerWithDescription)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO (stderr)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Types
import System.Environment (lookupEnv)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))

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

logError :: Text -> IO ()
logError = logJson "ERROR"

-- ---------------- FIRESTORE REST API HELPERS --------------------------------

data FirestoreDocument = FirestoreDocument
  { docName   :: Text
  , docFields :: Object
  } deriving (Show, Generic)

instance FromJSON FirestoreDocument where
  parseJSON = withObject "FirestoreDocument" $ \v ->
    FirestoreDocument <$> v .: "name" <*> v .: "fields"

instance ToJSON FirestoreDocument where
  toJSON (FirestoreDocument n f) = object ["name" .= n, "fields" .= f]

toFirestoreValue :: Value -> Value
toFirestoreValue (String s) = object ["stringValue" .= s]
toFirestoreValue (Number n) = object ["doubleValue" .= n]
toFirestoreValue (Bool b)   = object ["booleanValue" .= b]
toFirestoreValue Null       = object ["nullValue" .= Null]
toFirestoreValue _          = object ["stringValue" .= ("unsupported" :: Text)]

-- Convert Product to Firestore fields
productToFields :: Product -> Object
productToFields p = KM.fromList
  [ ("name", toFirestoreValue (String $ name p))
  , ("price", toFirestoreValue (Number $ realToFrac $ price p))
  , ("quantity", object ["integerValue" .= (show (quantity p) :: String)])
  , ("imgfile", toFirestoreValue (String $ imgfile p))
  , ("timestamp", object ["timestampValue" .= iso8601Show (timestamp p)])
  , ("actualdateadded", object ["timestampValue" .= iso8601Show (actualdateadded p)])
  ]

-- Convert Firestore document to Product
docToProduct :: FirestoreDocument -> Maybe Product
docToProduct doc = flip parseMaybe (docFields doc) $ \obj -> do
  n <- (obj .: "name") >>= (.: "stringValue")
  p <- (obj .: "price") >>= (.: "doubleValue")
  qStr <- (obj .: "quantity") >>= (.: "integerValue")
  let q = fromMaybe 0 (readMaybe qStr)
  img <- (obj .: "imgfile") >>= (.: "stringValue")
  tsStr <- (obj .: "timestamp") >>= (.: "timestampValue")
  -- Simple parse for ISO8601. In a real app use a better parser.
  -- iso8601Show produces something like "2023-01-01T00:00:00Z"
  -- We'll assume it's valid for now or use readMaybe if it works for UTCTime
  let ts = fromMaybe (read "2000-01-01 00:00:00 UTC") (readMaybe (T.unpack (T.replace "T" " " (T.replace "Z" " UTC" tsStr))))
  adStr <- (obj .: "actualdateadded") >>= (.: "timestampValue")
  let ad = fromMaybe (read "2000-01-01 00:00:00 UTC") (readMaybe (T.unpack (T.replace "T" " " (T.replace "Z" " UTC" adStr))))
  
  let pId = Just $ T.takeWhileEnd (/= '/') (docName doc)
  pure $ Product pId n p q img ts ad

-- ---------------- FIRESTORE API CALLS ---------------------------------------

getProjectId :: IO Text
getProjectId = T.pack . fromMaybe "mock-project" <$> lookupEnv "GOOGLE_CLOUD_PROJECT"

getFirestoreBaseUrl :: IO String
getFirestoreBaseUrl = do
  proj <- getProjectId
  pure $ "https://firestore.googleapis.com/v1/projects/" <> T.unpack proj <> "/databases/(default)/documents/inventory"

-- Simplified GET all documents
fetchProducts :: IO (Either Text [Product])
fetchProducts = do
  manager <- newManager tlsManagerSettings
  baseUrl <- getFirestoreBaseUrl
  initialRequest <- parseRequest baseUrl
  let request = initialRequest { method = "GET" }
  
  res <- try $ httpLbs request manager
  case res of
    Left (e :: SomeException) -> pure $ Left $ T.pack (show e)
    Right response ->
      let body = responseBody response
      in case decode body of
        Just (obj :: Object) -> 
          let docsValue = fromMaybe (Array mempty) (KM.lookup (K.fromString "documents") obj)
          in case fromJSON docsValue of
            Success (docs :: [FirestoreDocument]) -> pure $ Right $ mapMaybe docToProduct docs
            _ -> pure $ Right []
        Nothing -> pure $ Left "Failed to decode Firestore response"

-- Simplified POST/PATCH
saveProduct :: Product -> IO (Either Text ())
saveProduct p = do
  manager <- newManager tlsManagerSettings
  baseUrl <- getFirestoreBaseUrl
  initialRequest <- parseRequest baseUrl
  let fields = productToFields p
  let body = object ["fields" .= fields]
  let request = initialRequest 
        { method = "POST"
        , requestBody = RequestBodyLBS (encode body)
        , requestHeaders = [("Content-Type", "application/json")]
        }
  
  res <- try $ httpLbs request manager
  case res of
    Left (e :: SomeException) -> pure $ Left $ T.pack (show e)
    Right response ->
      if statusCode (responseStatus response) < 300
        then pure $ Right ()
        else pure $ Left $ "Firestore error: " <> T.pack (show (responseStatus response))

-- ---------------- TOOL HANDLERS ---------------------------------------------

$(return [])

handleMyTool :: MyTool -> IO Content
handleMyTool (Greet n) = do
  logInfo $ "Greeting " <> n
  pure $ ContentText $ "Hello, " <> n <> "!"

handleMyTool GetProducts = do
  logInfo "Fetching all products"
  res <- fetchProducts
  case res of
    Left err -> pure $ ContentText $ "Error: " <> err
    Right products -> pure $ ContentText $ T.pack $ BSL.unpack $ encode products

handleMyTool (GetProductById pid) = do
  logInfo $ "Fetching product by ID: " <> pid
  manager <- newManager tlsManagerSettings
  baseUrl <- getFirestoreBaseUrl
  initialRequest <- try $ parseRequest (baseUrl <> "/" <> T.unpack pid)
  case initialRequest of
    Left (e :: SomeException) -> pure $ ContentText $ "Error: " <> T.pack (show e)
    Right req -> do
      res <- try $ httpLbs req manager
      case res of
        Left (e :: SomeException) -> pure $ ContentText $ "Error: " <> T.pack (show e)
        Right response ->
          case decode (responseBody response) of
            Just (doc :: FirestoreDocument) ->
              case docToProduct doc of
                Just p -> pure $ ContentText $ T.pack $ BSL.unpack $ encode p
                Nothing -> pure $ ContentText "Error: Failed to parse product"
            Nothing -> pure $ ContentText "Error: Product not found"

handleMyTool (Search q) = do
  logInfo $ "Searching products with query: " <> q
  res <- fetchProducts
  case res of
    Left err -> pure $ ContentText $ "Error: " <> err
    Right products -> 
      let filtered = filter (\p -> T.toLower q `T.isInfixOf` T.toLower (name p)) products
      in pure $ ContentText $ T.pack $ BSL.unpack $ encode filtered

handleMyTool Seed = do
  logInfo "Seeding database"
  now <- getCurrentTime
  let productsList = 
        [ "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese"
        , "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice"
        , "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli"
        , "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts"
        , "Sunflower Seeds", "Fresh Basil", "Cinnamon"
        ]
  let seedProducts = flip map productsList $ \n -> Product
        { productId = Nothing
        , name = n
        , price = 2.5 -- Simplified
        , quantity = 100
        , imgfile = T.replace " " "" (T.toLower n) <> ".png"
        , timestamp = now
        , actualdateadded = now
        }
  results <- mapM saveProduct seedProducts
  let errors = [e | Left e <- results]
  if null errors
    then pure $ ContentText $ "Database seeded successfully with " <> T.pack (show (length productsList)) <> " products."
    else pure $ ContentText $ "Errors during seeding: " <> T.intercalate ", " errors

handleMyTool Reset = do
  logInfo "Resetting database"
  res <- fetchProducts
  case res of
    Left err -> pure $ ContentText $ "Error fetching products for reset: " <> err
    Right products -> do
      manager <- newManager tlsManagerSettings
      baseUrl <- getFirestoreBaseUrl
      let deleteProduct p = case productId p of
            Nothing -> pure (Right ())
            Just pid -> do
              initialRequest <- parseRequest (baseUrl <> "/" <> T.unpack pid)
              let request = initialRequest { method = "DELETE" }
              resDel <- try $ httpLbs request manager
              case resDel of
                Left (e :: SomeException) -> pure $ Left $ T.pack (show e)
                Right response ->
                  if statusCode (responseStatus response) < 300
                    then pure $ Right ()
                    else pure $ Left $ "Delete error: " <> T.pack (show (responseStatus response))
      
      results <- mapM deleteProduct products
      let errors = [e | Left e <- results]
      if null errors
        then pure $ ContentText $ "Database reset successfully. Deleted " <> T.pack (show (length products)) <> " products."
        else pure $ ContentText $ "Errors during reset: " <> T.intercalate ", " errors

handleMyTool GetRoot = do
  pure $ ContentText "🍎 Hello! This is the Cymbal Superstore Inventory API (Haskell)."

-- 3. Use deriveToolHandlerWithDescription to generate the tool handlers.
myToolHandlers :: (IO [ToolDefinition], ToolCallHandler IO)
myToolHandlers = $(deriveToolHandlerWithDescription ''MyTool 'handleMyTool myToolDescriptions)

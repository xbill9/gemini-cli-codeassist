{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Logic
  ( handleMyTool
  , myToolHandlers
  , logInfo
  )
where

import MCP.Server
import MCP.Server.Derive (deriveToolHandlerWithDescription)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), encode, decode)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import System.IO (stderr)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Types

import Text.Read (readMaybe)
import System.Info (os, arch, compilerName, compilerVersion)
import Data.Version (showVersion)
import System.Environment (lookupEnv)
import System.Random (randomRIO)

import Control.Lens ((.~), (<&>), (^.), (&), (?~), view, ix)
import Control.Applicative ((<|>))
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)

import Gogol
import qualified Gogol.FireStore as Firestore
import qualified Gogol.Data.Time as GTime

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

-- Firestore Helpers

getFirestoreEnv :: IO (Env '[Firestore.Datastore'FullControl])
getFirestoreEnv = do
  logger <- newLogger Error stderr
  newEnv <&> (envLogger .~ logger)

collectionPath :: Text -> Text
collectionPath projId = "projects/" <> projId <> "/databases/(default)/documents/inventory"

getProjectId :: IO Text
getProjectId = do
  mProj <- lookupEnv "GOOGLE_CLOUD_PROJECT"
  case mProj of
    Just p -> pure $ T.pack p
    Nothing -> do
      logError "GOOGLE_CLOUD_PROJECT environment variable not set."
      pure "unknown-project"

-- | Convert a Firestore Document to our Product type
docToProduct :: Firestore.Document -> Maybe Product
docToProduct doc = do
  fields <- doc.fields
  let fieldsMap = fields.additional
  
  let name = getString fieldsMap "name"
  let price = getDouble fieldsMap "price"
  let quantity = getInt fieldsMap "quantity"
  let img = getString fieldsMap "imgfile"
  let ts = getTime fieldsMap "timestamp"
  let added = getTime fieldsMap "actualdateadded"
  
  -- Extract ID from document name: projects/.../documents/inventory/ID
  let fullPath = doc.name
  let docId = listToMaybe $ reverse $ T.splitOn "/" (fromMaybe "" fullPath)

  pure $ Product
    { prodId = docId
    , prodName = name
    , prodPrice = price
    , prodQuantity = quantity
    , prodImgfile = img
    , prodTimestamp = ts
    , prodActualdateadded = added
    }
  where
    getString :: HashMap Text Firestore.Value -> Text -> Text
    getString fs k = fromMaybe "" $ do
        v <- Map.lookup k fs
        v.stringValue

    getDouble :: HashMap Text Firestore.Value -> Text -> Double
    getDouble fs k = fromMaybe 0 $ do
        v <- Map.lookup k fs
        v.doubleValue <|> (fromIntegral <$> v.integerValue)

    getInt :: HashMap Text Firestore.Value -> Text -> Int
    getInt fs k = fromMaybe 0 $ do
        v <- Map.lookup k fs
        fromIntegral <$> v.integerValue

    getTime :: HashMap Text Firestore.Value -> Text -> UTCTime
    getTime fs k = fromMaybe (read "1970-01-01 00:00:00 UTC") $ do
        v <- Map.lookup k fs
        dt <- v.timestampValue
        pure $ GTime.unDateTime dt

-- | Convert a Product to Firestore Document (fields)
productToFields :: Product -> HashMap Text Firestore.Value
productToFields p = Map.fromList
  [ ("name", mkStr (prodName p))
  , ("price", mkDouble (prodPrice p))
  , ("quantity", mkInt (prodQuantity p))
  , ("imgfile", mkStr (prodImgfile p))
  , ("timestamp", mkTime (prodTimestamp p))
  , ("actualdateadded", mkTime (prodActualdateadded p))
  ]
  where 
    mkStr s = (Firestore.newValue :: Firestore.Value) { Firestore.stringValue = Just s }
    mkDouble d = (Firestore.newValue :: Firestore.Value) { Firestore.doubleValue = Just d }
    mkInt i = (Firestore.newValue :: Firestore.Value) { Firestore.integerValue = Just (fromIntegral i) }
    mkTime t = (Firestore.newValue :: Firestore.Value) { Firestore.timestampValue = Just (GTime.DateTime t) }

-- | Empty TH splice
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

handleMyTool GetHaskellSystemInfo = do
  logInfo "Getting system info"
  let info = object
        [ "os" .= os
        , "arch" .= arch
        , "compiler" .= compilerName
        , "compiler_version" .= showVersion compilerVersion
        ]
  pure $ ContentText $ TE.decodeUtf8 $ BSL.toStrict $ encode info

handleMyTool GetServerInfo = do
  logInfo "Getting server info"
  pure $ ContentText "Server: firestore-stdio-haskell\nVersion: 0.1.0\nInstructions: A simple Haskell MCP server"

handleMyTool (Fibonacci n) = do
  logInfo $ "Generating " <> T.pack (show n) <> " Fibonacci numbers"
  if n < 0 then
    pure $ ContentText "Error: Count must be non-negative"
  else do
    let fibs = take n fibSequence
    pure $ ContentText $ T.pack $ show fibs

-- FIRESTORE TOOLS

handleMyTool GetProducts = do
  logInfo "GetProducts called"
  projId <- getProjectId
  env <- getFirestoreEnv
  
  let parent = "projects/" <> projId <> "/databases/(default)/documents"
  let req = Firestore.newFireStoreProjectsDatabasesDocumentsList parent "inventory"

  result <- runResourceT $ send env req
  
  -- result is ListDocumentsResponse
  -- result.documents :: Maybe [Document]
  let docs = fromMaybe [] result.documents
  let products = map docToProduct docs
  let validProducts = [p | Just p <- products]
  
  pure $ ContentText $ TE.decodeUtf8 $ BSL.toStrict $ encode validProducts

handleMyTool (GetProductById pid) = do
  logInfo $ "GetProductById called for " <> pid
  projId <- getProjectId
  env <- getFirestoreEnv
  
  let resourceName = "projects/" <> projId <> "/databases/(default)/documents/inventory/" <> pid
  let req = Firestore.newFireStoreProjectsDatabasesDocumentsGet resourceName
  
  -- Catch 404? runGoogle throws exception on 404.
  -- We should probably try catch it, but for now let it fail (MCP handles errors).
  result <- runResourceT $ send env req
  
  case docToProduct result of
    Just p -> pure $ ContentText $ TE.decodeUtf8 $ BSL.toStrict $ encode p
    Nothing -> pure $ ContentText "Error: Could not parse document"

handleMyTool CheckDb = do
  logInfo "CheckDb called"
  projId <- getProjectId
  env <- getFirestoreEnv
  let parent = "projects/" <> projId <> "/databases/(default)/documents"
  
  let req = (Firestore.newFireStoreProjectsDatabasesDocumentsList parent "inventory" :: Firestore.FireStoreProjectsDatabasesDocumentsList) { Firestore.pageSize = Just 1 }
  
  _ <- runResourceT $ send env req
  
  pure $ ContentText "Database running: true"

handleMyTool GetRoot = do
  pure $ ContentText "Apple Hello! This is the Cymbal Superstore Inventory API."

handleMyTool Reset = do
  logInfo "Reset called"
  projId <- getProjectId
  env <- getFirestoreEnv
  
  let parent = "projects/" <> projId <> "/databases/(default)/documents"
  let listReq = Firestore.newFireStoreProjectsDatabasesDocumentsList parent "inventory"
  
  result <- runResourceT $ send env listReq
  let docs = fromMaybe [] result.documents
  
  forM_ docs $ \doc -> do
    case doc.name of
      Just name -> do
        let delReq = Firestore.newFireStoreProjectsDatabasesDocumentsDelete name
        void $ runResourceT $ send env delReq
      Nothing -> pure ()
      
  pure $ ContentText "Database reset successfully."

handleMyTool Seed = do
  logInfo "Seed called"
  projId <- getProjectId
  env <- getFirestoreEnv
  
  now <- getCurrentTime
  
  let oldProducts = 
        [ "Apples", "Bananas", "Milk", "Whole Wheat Bread", "Eggs", "Cheddar Cheese"
        , "Whole Chicken", "Rice", "Black Beans", "Bottled Water", "Apple Juice"
        , "Cola", "Coffee Beans", "Green Tea", "Watermelon", "Broccoli"
        , "Jasmine Rice", "Yogurt", "Beef", "Shrimp", "Walnuts"
        , "Sunflower Seeds", "Fresh Basil", "Cinnamon"
        ]
        
  forM_ oldProducts $ \name -> do
    price <- randomRIO (1, 10) :: IO Double
    qty <- randomRIO (1, 500) :: IO Int
    let img = "product-images/" <> T.toLower (T.replace " " "" name) <> ".png"
    
    randTime <- randomRIO (0, 31536000) :: IO Double
    let ts = addUTCTime (realToFrac $ negate (randTime + 7776000)) now
    
    let p = Product Nothing name price qty img ts now
    addOrUpdateFirestore env projId p

  let recentProducts = 
        [ "Parmesan Crisps", "Pineapple Kombucha", "Maple Almond Butter"
        , "Mint Chocolate Cookies", "White Chocolate Caramel Corn", "Acai Smoothie Packs"
        , "Smores Cereal", "Peanut Butter and Jelly Cups"
        ]

  forM_ recentProducts $ \name -> do
    price <- randomRIO (1, 10) :: IO Double
    qty <- randomRIO (1, 100) :: IO Int
    let img = "product-images/" <> T.toLower (T.replace " " "" name) <> ".png"
    
    randTime <- randomRIO (0, 518400) :: IO Double
    let ts = addUTCTime (realToFrac $ negate randTime) now
    
    let p = Product Nothing name price qty img ts now
    addOrUpdateFirestore env projId p

  let recentProductsOutOfStock = ["Wasabi Party Mix", "Jalapeno Seasoning"]
  
  forM_ recentProductsOutOfStock $ \name -> do
    price <- randomRIO (1, 10) :: IO Double
    let qty = 0
    let img = "product-images/" <> T.toLower (T.replace " " "" name) <> ".png"
    
    randTime <- randomRIO (0, 518400) :: IO Double
    let ts = addUTCTime (realToFrac $ negate randTime) now
    
    let p = Product Nothing name price qty img ts now
    addOrUpdateFirestore env projId p

  pure $ ContentText "Database seeded successfully."


-- Helper to Add or Update based on Name
-- addOrUpdateFirestore :: Env -> Text -> Product -> IO ()
addOrUpdateFirestore env projId prod = do
  let parent = "projects/" <> projId <> "/databases/(default)/documents"
  
  -- Construct StructuredQuery
  
  let fieldRef = (Firestore.newFieldReference :: Firestore.FieldReference) { Firestore.fieldPath = Just "name" }
  let valStr = (Firestore.newValue :: Firestore.Value) { Firestore.stringValue = Just (prodName prod) }
  
  let fieldFilt = (Firestore.newFieldFilter :: Firestore.FieldFilter)
        { Firestore.field = Just fieldRef
        , Firestore.op = Just Firestore.FieldFilter_Op_Equal
        , Firestore.value = Just valStr 
        }
        
  let filt = (Firestore.newFilter :: Firestore.Filter) { Firestore.fieldFilter = Just fieldFilt }
  
  let colSel = (Firestore.newCollectionSelector :: Firestore.CollectionSelector) { Firestore.collectionId = Just "inventory" }
  
  let sq = (Firestore.newStructuredQuery :: Firestore.StructuredQuery)
        { Firestore.where' = Just filt
        , Firestore.from = Just [colSel]
        }

  let runQ = Firestore.newFireStoreProjectsDatabasesDocumentsRunQuery 
        parent
        ((Firestore.newRunQueryRequest :: Firestore.RunQueryRequest) { Firestore.structuredQuery = Just sq }) 
  
  result <- runResourceT $ send env runQ
  
  let existingDocs = [d | r <- [result], Just d <- [r.document]] -- Safe for singular.
  
  if null existingDocs then do
    logInfo $ "Adding new product: " <> prodName prod
    
    let fieldsMap = productToFields prod
    let dFields = Firestore.newDocument_Fields fieldsMap
    let doc = (Firestore.newDocument :: Firestore.Document) { Firestore.fields = Just dFields }
    
    let createReq = Firestore.newFireStoreProjectsDatabasesDocumentsCreateDocument parent "inventory" doc
    void $ runResourceT $ send env createReq
  else do
    logInfo $ "Updating product: " <> prodName prod
    forM_ existingDocs $ \d -> do
        case d.name of
            Just name -> do
                let fieldsMap = productToFields prod
                let dFields = Firestore.newDocument_Fields fieldsMap
                let patchDoc = (Firestore.newDocument :: Firestore.Document) { Firestore.fields = Just dFields }
                
                let patchReq = Firestore.newFireStoreProjectsDatabasesDocumentsPatch name patchDoc
                void $ runResourceT $ send env patchReq
            Nothing -> pure ()

fibSequence :: [Integer]
fibSequence = 0 : 1 : zipWith (+) fibSequence (drop 1 fibSequence)

-- 3. Use deriveToolHandlerWithDescription to generate the tool handlers.
myToolHandlers :: (IO [ToolDefinition], ToolCallHandler IO)
myToolHandlers = $(deriveToolHandlerWithDescription ''MyTool 'handleMyTool myToolDescriptions)

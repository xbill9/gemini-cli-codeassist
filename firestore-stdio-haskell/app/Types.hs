{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyTool(..)
  , myToolDescriptions
  , Product(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

-- 1. Define a data type for your tool's actions and parameters.
data MyTool
  = Greet { name :: Text }
  | Sum { values :: Text }
  | GetHaskellSystemInfo
  | GetServerInfo
  | Fibonacci { count :: Int }
  | GetProducts
  | GetProductById { productId :: Text }
  | Seed
  | Reset
  | CheckDb
  | GetRoot
  deriving (Show, Generic)

instance FromJSON MyTool
instance ToJSON MyTool

-- Descriptions for the tools and their arguments
myToolDescriptions :: [(String, String)]
myToolDescriptions =
  [ ("Greet", "Greet a person by name")
  , ("Sum", "Sum a list of numbers provided as a comma-separated string")
  , ("GetHaskellSystemInfo", "Get detailed Haskell system information (OS, Arch, Compiler)")
  , ("GetServerInfo", "Get information about this MCP server")
  , ("Fibonacci", "Get the first N numbers of the infinite Fibonacci sequence")
  , ("GetProducts", "Get all products from the inventory database")
  , ("GetProductById", "Get a single product from the inventory database by its ID")
  , ("Seed", "Seed the inventory database with products")
  , ("Reset", "Clears all products from the inventory database")
  , ("CheckDb", "Checks if the inventory database is running")
  , ("GetRoot", "Get a greeting from the Cymbal Superstore Inventory API")
  , ("name", "The name of the person to greet")
  , ("values", "A comma-separated list of integers (e.g., '1,2,3')")
  , ("count", "The number of Fibonacci numbers to generate")
  , ("productId", "The ID of the product to get")
  ]

data Product = Product
  { prodId :: Maybe Text
  , prodName :: Text
  , prodPrice :: Double
  , prodQuantity :: Int
  , prodImgfile :: Text
  , prodTimestamp :: UTCTime
  , prodActualdateadded :: UTCTime
  } deriving (Show, Generic)

instance ToJSON Product where
  toJSON p = object
    [ "id" .= prodId p
    , "name" .= prodName p
    , "price" .= prodPrice p
    , "quantity" .= prodQuantity p
    , "imgfile" .= prodImgfile p
    , "timestamp" .= prodTimestamp p
    , "actualdateadded" .= prodActualdateadded p
    ]

instance FromJSON Product where
  parseJSON = withObject "Product" $ \v -> Product
    <$> v .:? "id"
    <*> v .: "name"
    <*> v .: "price"
    <*> v .: "quantity"
    <*> v .: "imgfile"
    <*> v .: "timestamp"
    <*> v .: "actualdateadded"
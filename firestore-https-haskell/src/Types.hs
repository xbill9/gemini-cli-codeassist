{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types
  ( MyTool(..)
  , Product(..)
  , myToolDescriptions
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Data.Time (UTCTime)

-- Product object definition matching index.ts
data Product = Product
  { productId       :: Maybe Text
  , name            :: Text
  , price           :: Double
  , quantity        :: Int
  , imgfile         :: Text
  , timestamp       :: UTCTime
  , actualdateadded :: UTCTime
  } deriving (Show, Generic, ToJSON, FromJSON)

-- 1. Define a data type for your tool's actions and parameters.
data MyTool
  = Greet { greetName :: Text }
  | GetProducts
  | GetProductById { getProductId :: Text }
  | Search { query :: Text }
  | Seed
  | Reset
  | GetRoot
  deriving (Show, Generic, FromJSON, ToJSON)

-- Descriptions for the tools and their arguments
myToolDescriptions :: [(String, String)]
myToolDescriptions =
  [ ("Greet", "Greet a person by name")
  , ("greetName", "The name of the person to greet")
  , ("GetProducts", "Get a list of all products from the inventory database")
  , ("GetProductById", "Get a single product from the inventory database by its ID")
  , ("getProductId", "The ID of the product to get")
  , ("Search", "Search for products in the inventory database by name")
  , ("query", "The search query to filter products by name")
  , ("Seed", "Seed the inventory database with products")
  , ("Reset", "Clear all products from the inventory database")
  , ("GetRoot", "Get a greeting from the Cymbal Superstore Inventory API")
  ]
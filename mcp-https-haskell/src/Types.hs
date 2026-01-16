{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyTool(..)
  , myToolDescriptions
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- 1. Define a data type for your tool's actions and parameters.
data MyTool
  = Greet { name :: Text }
  | Sum { values :: Text }
  deriving (Show, Generic)

instance FromJSON MyTool
instance ToJSON MyTool

-- Descriptions for the tools and their arguments
myToolDescriptions :: [(String, String)]
myToolDescriptions =
  [ ("Greet", "Greet a person by name")
  , ("Sum", "Sum a list of numbers provided as a comma-separated string")
  , ("name", "The name of the person to greet")
  , ("values", "A comma-separated list of integers (e.g., '1,2,3')")
  ]

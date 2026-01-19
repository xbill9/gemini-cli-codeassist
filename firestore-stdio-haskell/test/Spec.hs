{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Logic
import Types
import qualified Data.Text as T
import MCP.Server (Content(..))

main :: IO ()
main = hspec $ do
  describe "Logic" $ do
    describe "handleMyTool" $ do
      it "returns a greeting" $ do
        result <- handleMyTool (Greet "World")
        result `shouldBe` ContentText "Hello, World!"

      it "sums numbers" $ do
        result <- handleMyTool (Sum "1,2,3")
        result `shouldBe` ContentText "6"

      it "handles invalid sum input" $ do
        result <- handleMyTool (Sum "1,a,3")
        result `shouldBe` ContentText "Error: Invalid input. Please provide a comma-separated list of integers."

      it "returns server info" $ do
        result <- handleMyTool GetServerInfo
        result `shouldBe` ContentText "Server: firestore-stdio-haskell\nVersion: 0.1.0\nInstructions: A simple Haskell MCP server"

      it "calculates fibonacci sequence" $ do
        result <- handleMyTool (Fibonacci 5)
        result `shouldBe` ContentText "[0,1,1,2,3]"

      it "checks database connection" $ do
        result <- handleMyTool CheckDb
        result `shouldBe` ContentText "Database running: true"

      it "seeds the database" $ do
        result <- handleMyTool Seed
        result `shouldBe` ContentText "Database seeded successfully."

      it "gets products" $ do
        result <- handleMyTool GetProducts
        case result of
          ContentText txt -> txt `shouldSatisfy` (\t -> "[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t)
          _ -> expectationFailure "Expected ContentText (JSON array)"
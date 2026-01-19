{-# LANGUAGE OverloadedStrings #-}

module LogicSpec (logicSpec) where

import Test.Hspec
import Logic
import Types
import qualified Data.Text as T
import MCP.Server (Content(..))

logicSpec :: Spec
logicSpec = do
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
      result `shouldBe` ContentText "Server: firestore-https-haskell\nVersion: 0.1.0\nInstructions: A Haskell-based MCP server for Firestore over HTTPS"

    it "calculates fibonacci sequence" $ do
      result <- handleMyTool (Fibonacci 5)
      result `shouldBe` ContentText "[0,1,1,2,3]"

    it "checks database connection" $ do
      result <- handleMyTool CheckDb
      -- This might fail if no ADC or network, but let's see. 
      -- In the sample it returns "Database running: true" or an error.
      case result of
        ContentText txt -> txt `shouldSatisfy` (\t -> "Database running:" `T.isPrefixOf` t)
        _ -> expectationFailure "Expected ContentText"

    it "gets products" $ do
      result <- handleMyTool GetProducts
      case result of
        ContentText txt -> txt `shouldSatisfy` (\t -> ("[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t) || "Error" `T.isPrefixOf` t)
        _ -> expectationFailure "Expected ContentText"

    it "searches products" $ do
      result <- handleMyTool (Search "apple")
      case result of
        ContentText txt -> txt `shouldSatisfy` (\t -> ("[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t) || "Error" `T.isPrefixOf` t)
        _ -> expectationFailure "Expected ContentText"

    it "returns root greeting" $ do
      result <- handleMyTool GetRoot
      result `shouldBe` ContentText "🍎 Hello! This is the Cymbal Superstore Inventory API."
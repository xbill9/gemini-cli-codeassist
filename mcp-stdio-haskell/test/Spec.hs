{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Logic
import Types
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

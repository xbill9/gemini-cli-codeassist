{-# LANGUAGE OverloadedStrings #-}

module LogicSpec (logicSpec) where

import Test.Hspec
import Logic
import Types
import MCP.Server (Content(..))

logicSpec :: Spec
logicSpec = do
  describe "handleMyTool" $ do
    it "greets a person" $ do
      result <- handleMyTool (Greet "Alice")
      result `shouldBe` ContentText "Hello, Alice!"

    it "sums a list of numbers" $ do
      result <- handleMyTool (Sum "1, 2, 3")
      result `shouldBe` ContentText "6"

    it "handles invalid sum input" $ do
      result <- handleMyTool (Sum "1, a, 3")
      result `shouldBe` ContentText "Error: Invalid input. Please provide a comma-separated list of integers."

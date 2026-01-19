module Main where

import Test.Hspec
import LogicSpec

main :: IO ()
main = hspec $ do
  logicSpec

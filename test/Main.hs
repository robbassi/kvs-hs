module Main where

import Test.Hspec
import qualified RBTreeSpec

main :: IO Bool
main = do
  hspec $ do
    RBTreeSpec.tests
  pure True

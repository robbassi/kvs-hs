module Main where

import qualified MemtableSpec
import qualified RBTreeSpec
import Test.Hspec

main :: IO Bool
main = do
  hspec $ do
    RBTreeSpec.tests
    MemtableSpec.tests
  pure True

module Main where

import Test.Hspec
import qualified RBTreeSpec
import qualified MemtableSpec

main :: IO Bool
main = do
  hspec $ do
    RBTreeSpec.tests
    MemtableSpec.tests
  pure True

module Main where

import qualified BinIOSpec
import qualified MemtableSpec
import qualified RBTreeSpec
import Test.Hspec

main :: IO Bool
main = do
  hspec $ do
    RBTreeSpec.tests
    MemtableSpec.tests
    BinIOSpec.tests
  pure True

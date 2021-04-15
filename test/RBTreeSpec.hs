module RBTreeSpec where

import Common
import Data.Foldable (for_)
import Data.List (sortOn)
import RBTree (RBTree)
import qualified RBTree
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck (Gen, Property, generate, property, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Types

buildTree :: Gen [Entry] -> IO ([(Key, Value)], RBTree)
buildTree input = do
  tree <- RBTree.empty
  input' <- generate input
  for_ input' (uncurry $ RBTree.insert tree)
  pure (input', tree)

prop_order :: Property
prop_order = monadicIO $ do
  (input, tree) <- run $ buildTree entriesUniqueByKey
  output <- run $ RBTree.toList tree
  let sortedInput = sortOn fst input
  assert $ output == sortedInput

prop_association :: Property
prop_association = monadicIO $ do
  (input, tree) <- run $ buildTree entriesWithDuplicateKeys
  entriesOut <- run $ RBTree.toList tree
  let entriesIn = latestEntries input
  assert $ entriesIn == entriesOut

tests :: SpecWith ()
tests = describe "RBTree" $ do
  it "insert maintains order of keys" $ do
    property $ withMaxSuccess 10000 prop_order
  it "search returns the correct values" $ do
    property $ withMaxSuccess 10000 prop_association

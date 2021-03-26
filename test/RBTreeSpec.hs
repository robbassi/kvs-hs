{-# options_ghc -Wno-unused-imports #-}

module RBTreeSpec where

import Types
import Orphans
import RBTree (RBTree)
import qualified RBTree
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck (Gen, Property, generate, arbitrary, property, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Data.List (sortOn, nubBy)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Function (on)
import Data.Maybe (fromJust)

entriesUniqueByKey :: Gen [(Key, Value)]
entriesUniqueByKey = nubBy ((==) `on` fst) <$> arbitrary

buildTree :: IO ([(Key, Value)], RBTree)
buildTree = do
  tree <- RBTree.empty
  input <- generate entriesUniqueByKey
  for_ input (uncurry $ RBTree.insert tree)
  pure (input, tree)

prop_order :: Property
prop_order = monadicIO $ do
  (input, tree) <- run buildTree
  output <- run $ RBTree.toList tree
  let sortedInput = sortOn fst input
  assert $ output == sortedInput

prop_association :: Property
prop_association = monadicIO $ do
  (input, tree) <- run buildTree
  valuesOut <- run $ for input (fmap fromJust . RBTree.search tree . fst)
  let valuesIn = snd <$> input
  assert $ valuesIn == valuesOut

tests :: SpecWith ()
tests =  describe "RBTree" $ do
  it "insert maintains order of keys" $ do
    property $ withMaxSuccess 10000 prop_order
  it "search returns the correct values" $ do
    property $ withMaxSuccess 10000 prop_association

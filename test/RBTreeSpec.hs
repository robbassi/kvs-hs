module RBTreeSpec where

import Types
import RBTree
import TestInstances
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.ByteString.Char8 (pack)
import Data.List (sortOn, nubBy)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Function (on)
import Data.Maybe (fromJust)

entriesUniqueByKey :: Gen [(Key, Value)]
entriesUniqueByKey = nubBy ((==) `on` fst) <$> arbitrary

prop_keyOrder :: Property
prop_keyOrder = monadicIO $ do
  (input, output) <- run $ do
    tree <- RBTree.empty
    input <- generate entriesUniqueByKey
    for_ input (uncurry $ RBTree.insert tree)
    entries <- RBTree.toList tree
    pure (input, entries)
  let sortedInput = sortOn fst input
  assert $ output == sortedInput

prop_keyAccess :: Property
prop_keyAccess = monadicIO $ do
  (input, valuesOut) <- run $ do
    tree <- RBTree.empty
    input <- generate entriesUniqueByKey
    for_ input (uncurry $ RBTree.insert tree)
    results <- for input (fmap fromJust . RBTree.search tree . fst)
    pure (input, results)
  let valuesIn = snd <$> input
  assert $ valuesIn == valuesOut

tests :: SpecWith ()
tests =  describe "RBTree" $ do
  it "maintains order of keys" $ do
    property $ withMaxSuccess 10000 prop_keyOrder
  it "search returns the correct values" $ do
    property $ withMaxSuccess 10000 prop_keyAccess

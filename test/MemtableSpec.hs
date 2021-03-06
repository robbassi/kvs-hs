module MemtableSpec where

import Common
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Memtable (Memtable)
import qualified Memtable
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck (Property, generate, property, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Types

buildMemtable :: IO ([Entry], Memtable)
buildMemtable = do
  memtable <- Memtable.empty
  input <- generate entriesWithDuplicateKeys
  for_ input (uncurry $ Memtable.set memtable)
  pure (input, memtable)

entrySize :: Entry -> Int
entrySize (key, Tombstone) = BS.length $ coerce key
entrySize (key, Value valueBytes) = keySize + valueSize
  where
    keySize = BS.length $ coerce key
    valueSize = BS.length valueBytes

prop_minByteCount :: Property
prop_minByteCount = monadicIO $ do
  (input, memtable) <- run buildMemtable
  let inputSize = sum $ entrySize <$> latestEntries input
  byteCount <- run $ Memtable.approximateBytes memtable
  assert $ byteCount == inputSize

prop_association :: Property
prop_association = monadicIO $ do
  (input, memtable) <- run buildMemtable
  valuesOut <- run $ Memtable.entries memtable
  assert $ latestEntries input == valuesOut

tests :: SpecWith ()
tests = describe "Memtable" $ do
  it "keeps track of memory usage" $ do
    property $ withMaxSuccess 10000 prop_minByteCount
  it "get returns the correct values" $ do
    property $ withMaxSuccess 10000 prop_association

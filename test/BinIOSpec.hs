module BinIOSpec where

import BinIO
import Common
import Data.Foldable (for_)
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck (Property, generate, property, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_roundtrip :: Property
prop_roundtrip = monadicIO $ do
  tmpFile <- run $ mkTempFilePath "kv-entries" ".bin"
  input <- run $ generate entriesUniqueByKey
  run $ withKVWriter tmpFile $ for_ input $ uncurry writeEntry
  output <- run (reverse <$> kvFold tmpFile (:) [])
  assert $ input == output

tests :: SpecWith ()
tests = describe "BinIO" $ do
  it "can read back data it has written" $ do
    property $ withMaxSuccess 1000 prop_roundtrip

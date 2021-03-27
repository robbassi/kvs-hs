{-# options_ghc -Wno-unused-imports #-}

module Common where

import Orphans
import Types (Key, Value)
import Test.QuickCheck (Gen, arbitrary)
import Data.Function (on)
import Data.List (nubBy)

entriesUniqueByKey :: Gen [(Key, Value)]
entriesUniqueByKey = nubBy ((==) `on` fst) <$> arbitrary

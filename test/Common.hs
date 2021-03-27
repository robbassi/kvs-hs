{-# OPTIONS_GHC -Wno-unused-imports #-}

module Common where

import Data.Function (on)
import Data.List (nubBy)
import Orphans
import Test.QuickCheck (Gen, arbitrary)
import Types (Key, Value)

entriesUniqueByKey :: Gen [(Key, Value)]
entriesUniqueByKey = nubBy ((==) `on` fst) <$> arbitrary

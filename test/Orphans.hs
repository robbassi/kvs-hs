{-# options_ghc 
    -Wno-unused-imports 
    -Wno-orphans
#-}

module Orphans where

import Types
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary

instance Arbitrary Value where
  arbitrary = frequency $ [(9, Value <$> arbitrary)
                          ,(1, pure Tombstone)]

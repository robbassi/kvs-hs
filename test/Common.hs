{-# OPTIONS_GHC -Wno-unused-imports #-}

module Common where

import Data.Function (on)
import Data.List (nubBy)
import Data.Word (Word64)
import Orphans
import System.CPUTime (getCPUTime)
import Test.QuickCheck (Gen, arbitrary, generate)
import Types (Key, Value)

entriesUniqueByKey :: Gen [(Key, Value)]
entriesUniqueByKey = nubBy ((==) `on` fst) <$> arbitrary

mkTempFilePath :: String -> String -> IO FilePath
mkTempFilePath prefix affix = do
  cpuTime <- getCPUTime
  randNumber <- generate arbitrary :: IO Word64
  pure $ "/tmp/" ++ prefix ++ "-" ++ show cpuTime ++ "-" ++ show randNumber ++ affix

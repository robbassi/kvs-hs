{-# OPTIONS_GHC -Wno-unused-imports #-}

module Common where

import Data.Function (on)
import Data.List (foldl', nubBy, sortOn)
import Data.Traversable (for)
import Data.Word (Word64)
import Orphans
import System.CPUTime (getCPUTime)
import Test.QuickCheck (Gen, arbitrary, generate)
import Test.QuickCheck.Gen (sublistOf)
import Types

entriesUniqueByKey :: Gen [Entry]
entriesUniqueByKey = nubBy ((==) `on` fst) <$> arbitrary

entriesWithDuplicateKeys :: Gen [Entry]
entriesWithDuplicateKeys = do
  entries <- arbitrary
  dups <- sublistOf entries
  updatedEntries <- for dups $ \(k, _) -> (k,) <$> arbitrary
  pure $ entries ++ updatedEntries

latestEntries :: [Entry] -> [Entry]
latestEntries = reverse . foldl' merge [] . sortOn fst
  where
    merge :: [Entry] -> Entry -> [Entry]
    merge (prev@(key, _) : rest) next@(key', _) =
      if key == key'
        then next : rest
        else next : prev : rest
    merge [] kv = [kv]

mkTempFilePath :: String -> String -> IO FilePath
mkTempFilePath prefix affix = do
  cpuTime <- getCPUTime
  randNumber <- generate arbitrary :: IO Word64
  pure $ "/tmp/" ++ prefix ++ "-" ++ show cpuTime ++ "-" ++ show randNumber ++ affix

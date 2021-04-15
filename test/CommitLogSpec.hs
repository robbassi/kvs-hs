module CommitLogSpec where

import BinIO (kvFold)
import qualified CommitLog
import Common
import Data.Foldable (for_)
import qualified Memtable
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck (Property, arbitrary, generate, property, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Types hiding (commitLogPath)

buildCommitLog :: IO ([Entry], FilePath)
buildCommitLog = do
  commitLogPath <- mkTempFilePath "commitlog" ".bin"
  (_, commitLog) <- CommitLog.resume commitLogPath
  input <- generate arbitrary :: IO [Entry]
  for_ input (uncurry $ CommitLog.set commitLog)
  CommitLog.close commitLog
  pure (input, commitLogPath)

prop_roundtrip :: Property
prop_roundtrip = monadicIO $ do
  (input, commitLogPath) <- run buildCommitLog
  entriesOut <- run $ reverse <$> kvFold commitLogPath (:) []
  (memtable, _) <- run $ CommitLog.resume commitLogPath
  memtableEntries <- run $ Memtable.entries memtable
  assert $ input == entriesOut
  assert $ latestEntries input == memtableEntries

tests :: SpecWith ()
tests = describe "CommitLog" $ do
  it "writes entries in order" $ do
    property $ withMaxSuccess 1000 prop_roundtrip

module Segments where

import Memtable
import Types (Key, Value)

type SSTable = ()

data Segments = Segments
  { compactionThread :: (),
    sstables :: [SSTable]
  }

fromPath :: FilePath -> IO Segments
fromPath _ = do
  -- load the segments from disk
  -- sort the segments
  pure $ Segments () []

search :: Segments -> Key -> IO (Maybe Value)
search = undefined

flush :: Segments -> Memtable -> IO ()
flush = undefined

startCompactionThread :: IO ()
startCompactionThread = undefined

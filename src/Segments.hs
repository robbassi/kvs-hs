module Segments where

import Types (Key, Value)
import Memtable

type SSTable = ()

data Segments =
  Segments { compactionThread :: ()
           , sstables :: [SSTable] }

fromPath :: String -> IO Segments
fromPath path = do
  -- load the segments from disk
  -- sort the segments
  pure $ Segments () []

search :: Segments -> Key -> IO (Maybe Value)
search = undefined

flush :: Segments -> Memtable ->  IO ()
flush = undefined

startCompactionThread :: IO ()
startCompactionThread = undefined

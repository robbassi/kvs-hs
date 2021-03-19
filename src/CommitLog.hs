module CommitLog where

import Memtable (Memtable, empty)
import Types (Key, Value)

type Writer = ()

data CommitLog = CommitLog { writer :: Writer }

resume :: String -> IO (Memtable, CommitLog)
resume path = do
  -- if path exists
  -- load the memtable
  -- return commit log
  pure $ (empty, CommitLog ())

set :: CommitLog -> Key -> Value -> IO ()
set = undefined

unset :: CommitLog -> Key -> IO ()
unset = undefined

purge :: CommitLog -> IO ()
purge = undefined

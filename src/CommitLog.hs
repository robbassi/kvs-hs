module CommitLog where

import BinIO
import Memtable (Memtable)
import qualified Memtable
import Types (Key, Value (..))
import Control.Monad (when)
import System.Posix.Files (fileExist)

data CommitLog = CommitLog {commitLogPath :: FilePath}

resume :: FilePath -> IO (Memtable, CommitLog)
resume commitLogPath = do
  memtable <- Memtable.empty
  exists <- fileExist commitLogPath
  when exists $ do
    let setEntry key Tombstone = Memtable.unset memtable key
        setEntry key value = Memtable.set memtable key value
    kvIterIO commitLogPath $ uncurry setEntry
  pure (memtable, CommitLog {..})

set :: CommitLog -> Key -> Value -> IO ()
set CommitLog {..} key value =
  withKVAppender commitLogPath $ do
    writeEntry key value
    sync

unset :: CommitLog -> Key -> IO ()
unset CommitLog {..} key =
  withKVAppender commitLogPath $ do
    writeEntry key Tombstone
    sync

purge :: CommitLog -> IO ()
purge CommitLog {..} =
  withKVAppender commitLogPath $ BinIO.truncate

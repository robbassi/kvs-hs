module CommitLog where

import BinIO
import Control.Monad (when)
import Memtable (Memtable)
import qualified Memtable
import System.IO (Handle, hClose)
import System.Posix.Files (fileExist)
import System.Posix.IO (FdOption (..), OpenMode (..), defaultFileFlags, fdToHandle, openFd, setFdOption)
import Types (Key, Value (..))

newtype CommitLog = CommitLog {commitLogHandle :: Handle}

resume :: FilePath -> IO (Memtable, CommitLog)
resume commitLogPath = do
  memtable <- Memtable.empty
  exists <- fileExist commitLogPath
  when exists $ recoverMemtable commitLogPath memtable
  commitLogHandle <- openCommitLog commitLogPath
  pure (memtable, CommitLog {..})

set :: CommitLog -> Key -> Value -> IO ()
set CommitLog {..} key value =
  withKVWriter' commitLogHandle $ do
    writeEntry key value
    sync

unset :: CommitLog -> Key -> IO ()
unset CommitLog {..} key =
  withKVWriter' commitLogHandle $ do
    writeEntry key Tombstone
    sync

purge :: CommitLog -> IO ()
purge CommitLog {..} =
  withKVWriter' commitLogHandle BinIO.truncate

close :: CommitLog -> IO ()
close CommitLog {..} = hClose commitLogHandle

recoverMemtable :: FilePath -> Memtable -> IO ()
recoverMemtable commitLogPath memtable = kvIterIO commitLogPath $ uncurry setEntry
  where
    setEntry key Tombstone = Memtable.unset memtable key
    setEntry key value = Memtable.set memtable key value

openCommitLog :: FilePath -> IO Handle
openCommitLog commitLogPath = do
  commitLogFd <- openFd commitLogPath WriteOnly Nothing defaultFileFlags
  setFdOption commitLogFd AppendOnWrite True
  setFdOption commitLogFd SynchronousWrites True
  fdToHandle commitLogFd

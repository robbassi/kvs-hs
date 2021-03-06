module CommitLog
  ( -- * Common API
    CommitLog,
    resume,
    set,
    purge,

    -- * Debugging
    close,
  )
where

import BinIO
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
  commitLogHandle <- openCommitLog commitLogPath memtable
  pure (memtable, CommitLog {..})

set :: CommitLog -> Key -> Value -> IO ()
set CommitLog {..} key value =
  withKVWriter' commitLogHandle $ do
    writeEntry key value
    sync

purge :: CommitLog -> IO ()
purge CommitLog {..} =
  withKVWriter' commitLogHandle BinIO.truncate

close :: CommitLog -> IO ()
close CommitLog {..} = hClose commitLogHandle

recoverMemtable :: FilePath -> Memtable -> IO ()
recoverMemtable commitLogPath memtable = kvIterIO commitLogPath $ uncurry $ Memtable.set memtable

openCommitLog :: FilePath -> Memtable -> IO Handle
openCommitLog commitLogPath memtable = do
  exists <- fileExist commitLogPath
  if exists
    then recoverMemtable commitLogPath memtable
    else writeFile commitLogPath mempty
  commitLogFd <- openFd commitLogPath WriteOnly Nothing defaultFileFlags
  setFdOption commitLogFd AppendOnWrite True
  setFdOption commitLogFd SynchronousWrites True
  fdToHandle commitLogFd

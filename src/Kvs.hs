module KVS where

import CommitLog
import Segments
import Types
import Memtable
import Data.IORef
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.ReadWriteLock (RWLock)
import qualified Control.Concurrent.ReadWriteLock as RWLock

mtMaxSize :: Int
mtMaxSize = 5000000

type SegmentPath = String

data KvsData =
  KvsData { commitLog :: CommitLog
          , memtable :: IORef Memtable
          , segments :: Segments
          , rwLock :: RWLock }

type Kvs = ReaderT KvsData IO

mkKvsData :: KvsConfig -> IO KvsData
mkKvsData KvsConfig {..} = do
  (memtable', commitLog) <- CommitLog.resume commitLogPath
  memtable <- newIORef memtable'
  segments <- Segments.fromPath segmentPath
  rwLock <- RWLock.new
  void $ Segments.startCompactionThread
  pure $ KvsData {..}

get :: Key -> Kvs (Maybe Value)
get k = do
  KvsData {..} <- ask
  lift $ RWLock.withRead rwLock $ do
    memtable' <- readIORef memtable
    Memtable.get memtable' k <|> Segments.search segments k

set :: Key -> Value -> Kvs ()
set k v = do
  kvsData@KvsData {..} <- ask
  lift $ RWLock.withWrite rwLock $ do
    memtable' <- readIORef memtable
    void $ CommitLog.set commitLog k v
    void $ Memtable.set memtable' k v
    byteCount <- Memtable.approximateBytes memtable'
    when (byteCount >= mtMaxSize) $
      flushMemory kvsData

unset :: Key -> Value -> Kvs ()
unset k v = do
  kvsData@KvsData {..} <- ask
  lift $ RWLock.withWrite rwLock $ do
    memtable' <- readIORef memtable
    void $ CommitLog.unset commitLog k
    void $ Memtable.unset memtable' k
    byteCount <- Memtable.approximateBytes memtable'
    when (byteCount >= mtMaxSize) $
      flushMemory kvsData

flushMemory :: KvsData -> IO ()
flushMemory KvsData {..} = do
  memtable' <- readIORef memtable
  void $ Segments.flush segments memtable'
  writeIORef memtable $ Memtable.empty
  void $ CommitLog.purge commitLog

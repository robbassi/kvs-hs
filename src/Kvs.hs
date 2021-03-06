module Kvs
  ( -- * KVS API
    Kvs,
    get,
    set,

    -- * Kvs State
    KvsData,
    mkKvsData,
  )
where

import CommitLog (CommitLog)
import qualified CommitLog
import Control.Applicative ((<|>))
import Control.Concurrent.ReadWriteLock (RWLock)
import qualified Control.Concurrent.ReadWriteLock as RWLock
import Control.Monad.Reader
import Data.IORef
import Memtable (Memtable)
import qualified Memtable
import Segments
import Types

mtMaxSize :: Int
mtMaxSize = 5000000

data KvsData = KvsData
  { commitLog :: CommitLog,
    memtable :: IORef Memtable,
    segments :: Segments,
    rwLock :: RWLock
  }

type Kvs = ReaderT KvsData IO

mkKvsData :: KvsConfig -> IO KvsData
mkKvsData KvsConfig {..} = do
  (memtable', commitLog) <- CommitLog.resume commitLogPath
  memtable <- newIORef memtable'
  segments <- Segments.fromPath segmentPath
  rwLock <- RWLock.new
  void Segments.startCompactionThread
  pure $ KvsData {..}

get :: Key -> Kvs (Maybe Value)
get k = do
  KvsData {..} <- ask
  lift $
    RWLock.withRead rwLock $ do
      memtable' <- readIORef memtable
      Memtable.get memtable' k <|> Segments.search segments k

set :: Key -> Value -> Kvs ()
set k v = do
  kvsData@KvsData {..} <- ask
  lift $
    RWLock.withWrite rwLock $ do
      memtable' <- readIORef memtable
      void $ CommitLog.set commitLog k v
      void $ Memtable.set memtable' k v
      byteCount <- Memtable.approximateBytes memtable'
      when (byteCount >= mtMaxSize) $
        flushMemory kvsData

flushMemory :: KvsData -> IO ()
flushMemory KvsData {..} = do
  memtable' <- readIORef memtable
  void $ Segments.flush segments memtable'
  writeIORef memtable =<< Memtable.empty
  void $ CommitLog.purge commitLog

module Memtable
  ( -- * Memtable API
    Memtable,
    Memtable.empty,
    get,
    entries,
    approximateBytes,
    set,
  )
where

import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import RBTree (RBTree)
import qualified RBTree
import Types (Entry, Key (..), Value (..))

data Memtable = Memtable
  { memBytes :: IORef Int,
    memTree :: IORef RBTree
  }

empty :: IO Memtable
empty = do
  memBytes <- newIORef 0
  memTree <- newIORef =<< RBTree.empty
  pure $ Memtable {..}

get :: Memtable -> Key -> IO (Maybe Value)
get Memtable {..} key = do
  tree <- readIORef memTree
  RBTree.search tree key

entries :: Memtable -> IO [Entry]
entries Memtable {..} = RBTree.toList =<< readIORef memTree

approximateBytes :: Memtable -> IO Int
approximateBytes Memtable {..} = readIORef memBytes

set :: Memtable -> Key -> Value -> IO ()
set Memtable {..} key value = do
  tree <- readIORef memTree
  mValue <- RBTree.search tree key
  atomicAdd memBytes $ sizeDiff mValue
  RBTree.insert tree key value
  where
    keySize = BS.length $ coerce key
    newValueSize = case value of
      Value valueBytes -> BS.length valueBytes
      Tombstone -> 0
    sizeDiff = \case
      Just Tombstone -> newValueSize
      Just (Value valueBytes) -> newValueSize - BS.length valueBytes
      Nothing -> keySize + newValueSize
    atomicAdd ref n = do
      atomicModifyIORef' ref (\n' -> (n' + n, ()))

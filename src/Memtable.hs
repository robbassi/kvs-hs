module Memtable
  ( Memtable,
    Memtable.empty,
    set,
    get,
    unset,
    entries,
    approximateBytes,
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

set :: Memtable -> Key -> Value -> IO ()
set Memtable {..} key value = do
  atomicAdd memBytes (keySize + valueSize)
  tree <- readIORef memTree
  RBTree.insert tree key value
  where
    keySize = BS.length $ coerce key
    valueSize = case value of
      Value valueBytes -> BS.length valueBytes
      Tombstone -> 0

get :: Memtable -> Key -> IO (Maybe Value)
get Memtable {..} key = do
  tree <- readIORef memTree
  RBTree.search tree key

unset :: Memtable -> Key -> IO ()
unset Memtable {..} key = do
  tree <- readIORef memTree
  mValue <- RBTree.search tree key
  case mValue of
    Just Tombstone -> pure ()
    Just (Value valueBytes) -> do
      let valueSize = BS.length valueBytes
      atomicAdd memBytes $ negate valueSize
    Nothing -> do
      let keySize = BS.length $ coerce key
      atomicAdd memBytes keySize
  RBTree.insert tree key Tombstone

entries :: Memtable -> IO [Entry]
entries Memtable {..} = RBTree.toList =<< readIORef memTree

approximateBytes :: Memtable -> IO Int
approximateBytes Memtable {..} = do
  bytes <- readIORef memBytes
  pure bytes

atomicAdd :: IORef Int -> Int -> IO ()
atomicAdd ref n = do
  atomicModifyIORef' ref (\n' -> (n' + n, ()))

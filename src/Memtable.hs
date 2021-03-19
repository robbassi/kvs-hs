module Memtable (
  Memtable,
  empty,
  set,
  get,
  unset,
  entries,
  approximateBytes
) where

import Types (Key, Value, Entry)

type BalancedTree k v = [(k,v)]

data Memtable =
  Memtable { bytes :: Int
           , tree :: BalancedTree Key Value }

empty :: Memtable
empty = Memtable 0 []

set :: Memtable -> Key -> Value -> IO ()
set = undefined

get :: Memtable -> Key -> IO (Maybe Value)
get = undefined

unset :: Memtable -> Key -> IO ()
unset = undefined

entries :: Memtable -> IO [Entry]
entries = undefined

approximateBytes :: Memtable -> IO Int
approximateBytes Memtable {..} = pure bytes

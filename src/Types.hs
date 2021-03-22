module Types where

import Data.ByteString (ByteString)

newtype Key = Key ByteString
  deriving (Show, Eq, Ord)

data Value = Value ByteString | Tombstone
  deriving (Show, Eq)

type Entry = (Key, Value)

data KvsConfig =
  KvsConfig { commitLogPath :: String
            , segmentPath :: String }

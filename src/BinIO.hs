{-# OPTIONS_GHC -Wno-orphans #-}

module BinIO
  ( -- * The KVReader API
    KVReader,
    withKVReader,
    hasNext,
    seek,
    skip,
    skipKey,
    skipValue,
    readKeySize,
    readValueSize,
    readKey,
    readValue,
    readEntry,

    -- * The KVWriter API
    KVWriter,
    withKVWriter,
    writeKey,
    writeValue,
    writeEntry,
    sync,
    BinIO.truncate,

    -- * Helpers
    kvFold,
    kvFoldIO,
    kvIterIO,
  )
where

import Control.Monad.Reader
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int
import System.IO
import System.Posix.IO (handleToFd)
import System.Posix.Unistd (fileSynchronise)
import Types

type KVReader = ReaderT Handle IO

type KVWriter = ReaderT Handle IO ()

tombstoneSize :: Int16
tombstoneSize = -1

kvFoldIO :: FilePath -> (Entry -> a -> IO a) -> a -> IO a
kvFoldIO path f = withKVReader path . loop
  where
    loop acc = do
      continue <- hasNext
      if continue
        then readEntry >>= liftIO . flip f acc >>= loop
        else pure acc

kvIterIO :: FilePath -> (Entry -> IO ()) -> IO ()
kvIterIO path f = kvFoldIO path (\kv _ -> f kv) ()

kvFold :: FilePath -> (Entry -> a -> a) -> a -> IO a
kvFold path f = withKVReader path . loop
  where
    loop acc = do
      continue <- hasNext
      if continue
        then readEntry >>= pure . flip f acc >>= loop
        else pure acc

-- KVReader API

withKVReader :: FilePath -> KVReader a -> IO a
withKVReader path = withFile path ReadMode . runReaderT

hasNext :: KVReader Bool
hasNext = ask >>= liftIO . hIsEOF >>= pure . not

seek :: Integer -> KVReader ()
seek offset = do
  handle <- ask
  liftIO $ hSeek handle AbsoluteSeek offset

skip :: Integer -> KVReader ()
skip offset = do
  handle <- ask
  liftIO $ hSeek handle RelativeSeek offset

readKeySize :: KVReader Int
readKeySize = do
  handle <- ask
  liftIO $ do
    sizeBytes <- BL.hGet handle 2
    pure $ fromEnum $ (decode sizeBytes :: Word16)

readValueSize :: KVReader Int
readValueSize = do
  handle <- ask
  liftIO $ do
    sizeBytes <- BL.hGet handle 2
    pure $ fromEnum $ (decode sizeBytes :: Int16)

skipKey :: KVReader ()
skipKey = readKeySize >>= skip . toInteger

skipValue :: KVReader ()
skipValue = readValueSize >>= skip . toInteger

readKey :: KVReader Key
readKey = do
  handle <- ask
  keySize <- readKeySize
  keyBytes <- liftIO $ BS.hGet handle keySize
  pure $ Key keyBytes

readValue :: KVReader Value
readValue = do
  handle <- ask
  liftIO $ do
    valueSize <- BL.hGet handle 2
    case decode valueSize of
      n | n == tombstoneSize -> pure Tombstone
      n -> Value <$> (BS.hGet handle $ fromEnum n)

readEntry :: KVReader Entry
readEntry = (,) <$> readKey <*> readValue

-- KVWriter API

withKVWriter :: FilePath -> KVWriter -> IO ()
withKVWriter path = withFile path WriteMode . runReaderT

writeKey :: Key -> KVWriter
writeKey (Key keyBytes) = do
  handle <- ask
  let keySize = toEnum $ BS.length keyBytes :: Word16
      keySizeBytes = encode keySize
  liftIO $ do
    BL.hPut handle keySizeBytes
    BS.hPut handle keyBytes

writeValue :: Value -> KVWriter
writeValue (Value valueBytes) = do
  handle <- ask
  let valueSize = toEnum $ BS.length valueBytes :: Int16
      valueSizeBytes = encode valueSize
  liftIO $ do
    BL.hPut handle valueSizeBytes
    BS.hPut handle valueBytes
writeValue Tombstone = ask >>= liftIO . flip BL.hPut tombstoneBytes
  where
    tombstoneBytes = encode tombstoneSize

writeEntry :: Key -> Value -> KVWriter
writeEntry key value = writeKey key >> writeValue value

sync :: KVWriter
sync = do
  handle <- ask
  liftIO $ do
    hFlush handle
    fd <- handleToFd handle
    fileSynchronise fd

truncate :: KVWriter
truncate = do
  handle <- ask
  liftIO $ do
    hSeek handle AbsoluteSeek 0
    hSetFileSize handle 0

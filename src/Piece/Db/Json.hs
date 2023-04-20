module Piece.Db.Json
  ( MonadReadJson,
    MonadWriteJson,
    readJson,
    writeJson,
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Piece.App.Monad as Monad
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

class Monad m => MonadReadJson m where
  readJson :: FromJSON a => FilePath -> m (Either SomeException a)

instance MonadReadJson Monad.App where
  readJson = readJsonImpl
  {-# INLINE readJson #-}

readJsonImpl :: (UnliftIO.MonadUnliftIO m, FromJSON a) => FilePath -> m (Either SomeException a)
readJsonImpl fp = UnliftIO.tryAny $ liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

class Monad m => MonadWriteJson m where
  writeJson :: ToJSON a => FilePath -> a -> m (Either SomeException ())

instance MonadWriteJson Monad.App where
  writeJson = writeJsonImpl
  {-# INLINE writeJson #-}

writeJsonImpl :: (ToJSON a, UnliftIO.MonadUnliftIO m) => FilePath -> a -> m (Either SomeException ())
writeJsonImpl fp items = UnliftIO.tryAny $ liftIO $ BS.writeFile fp $ toStrict $ encode items

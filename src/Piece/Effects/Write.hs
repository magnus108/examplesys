module Piece.Effects.Write
  ( MonadWrite (..),
    writeImpl,
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified UnliftIO

class (ToJSON a, Monad m) => MonadWrite m a where
  write :: FilePath -> a -> m (Either () ())

instance ToJSON a => MonadWrite Monad.App a where
  write = writeImpl
  {-# INLINE write #-}

writeImpl ::
  ( ToJSON a,
    UnliftIO.MonadUnliftIO m
  ) =>
  FilePath ->
  a ->
  m (Either () ())
writeImpl datastore s = do
  write <- UnliftIO.tryAny $ liftIO $ BS.writeFile datastore $ toStrict $ encode s
  case write of
    Left _ -> return (Left ())
    Right x -> return (Right x)

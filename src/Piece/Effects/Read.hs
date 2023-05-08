module Piece.Effects.Read
  ( MonadRead,
    read,
    readImpl,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, decode, throwDecodeStrict)
import qualified Data.ByteString as BS
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified UnliftIO

class (FromJSON a, Monad m) => MonadRead m a where
  read :: FilePath -> m (Either () a)

instance FromJSON a => MonadRead Monad.App a where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (FromJSON a, UnliftIO.MonadUnliftIO m) => FilePath -> m (Either () a)
readImpl datastore = do
  read <- UnliftIO.tryAny $ liftIO $ throwDecodeStrict =<< BS.readFile datastore
  case read of
    Right x -> return (Right x)
    Left y -> return (Left ())

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
  read :: FilePath -> m a

instance FromJSON a => MonadRead Monad.App a where
  read = readImplSafe
  {-# INLINE read #-}

readImpl :: (MonadIO m, FromJSON a) => FilePath -> m a
readImpl datastore = liftIO $ throwDecodeStrict =<< BS.readFile datastore

readImplSafe :: (FromJSON a, UnliftIO.MonadUnliftIO m, E.As err E.UserError, E.WithError err m) => FilePath -> m a
readImplSafe datastore = do
  read' <- UnliftIO.tryAny $ readImpl datastore
  case read' of
    Right (Just x) -> return x
    _ -> Error.throwError (E.as E.NotFound)

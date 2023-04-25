module Piece.Effects.Read
  ( MonadRead,
    read,
  )
where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString as BS
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

class (FromJSON a, Monad m) => MonadRead m a where
  read :: FilePath -> m a

instance FromJSON a => MonadRead Monad.App a where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (FromJSON a, UnliftIO.MonadUnliftIO m, E.As err E.UserError, E.WithError err m) => FilePath -> m a
readImpl datastore = do
  read' <- UnliftIO.tryAny $ liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore
  case read' of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right x -> return x

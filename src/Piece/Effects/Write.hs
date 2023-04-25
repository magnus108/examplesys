module Piece.Effects.Write
  ( MonadWrite (..),
    writeImpl,
    writeImplSafe,
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified UnliftIO

class (ToJSON a, Monad m) => MonadWrite m a where
  write :: FilePath -> a -> m ()

instance ToJSON a => MonadWrite Monad.App a where
  write = writeImplSafe
  {-# INLINE write #-}

writeImpl :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeImpl datastore s = liftIO $ BS.writeFile datastore $ toStrict $ encode s

writeImplSafe ::
  ( E.As err E.UserError,
    E.WithError err m,
    ToJSON a,
    UnliftIO.MonadUnliftIO m
  ) =>
  FilePath ->
  a ->
  m ()
writeImplSafe datastore s = do
  write' <- UnliftIO.tryAny $ writeImpl datastore s
  case write' of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

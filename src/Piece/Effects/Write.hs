module Piece.Effects.Write
  ( MonadWrite (..),
  )
where

import qualified Control.Monad.IO.Unlift as Unlift
import Data.Aeson
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as BS
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

class (ToJSON a, Monad m) => MonadWrite m a where
  write :: FilePath -> a -> m ()

instance ToJSON a => MonadWrite Monad.App a where
  write = writeImpl
  {-# INLINE write #-}

writeImpl ::
  ( E.As err E.UserError,
    E.WithError err m,
    ToJSON a,
    UnliftIO.MonadUnliftIO m
  ) =>
  FilePath ->
  a ->
  m ()
writeImpl datastore s = do
  write' <- UnliftIO.tryAny $ liftIO $ BS.writeFile datastore $ toStrict $ encode s
  case write' of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

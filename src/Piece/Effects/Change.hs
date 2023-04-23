module Piece.Effects.Change
  ( MonadChanges (..),
  )
where

import qualified Control.Monad.IO.Unlift as Unlift
import Data.Aeson.Types (ToJSON)
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.Db.Json as Json
import qualified Reactive.Threepenny as R

class (ToJSON a, Monad m) => MonadChanges m a where
  listen :: FilePath -> a -> m ()

instance ToJSON a => MonadChanges Monad.App a where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl ::
  ( E.As err E.UserError,
    E.WithError err m,
    Json.MonadWriteJson m,
    ToJSON a
  ) =>
  FilePath ->
  a ->
  m ()
listenImpl datastore s = do
  dataWrite <- Json.writeJson datastore s
  case dataWrite of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

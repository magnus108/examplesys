module Piece.Effects.Read
  ( MonadRead,
    read,
  )
where

import Data.Aeson (FromJSON)
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.Db.Json as Json

class (FromJSON a, Monad m) => MonadRead m a where
  read :: FilePath -> m a

instance FromJSON a => MonadRead Monad.App a where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (FromJSON a, E.As err E.UserError, E.WithError err m, Json.MonadReadJson m) => FilePath -> m a
readImpl datastoreLoan = do
  databaseLoan <- Json.readJson datastoreLoan
  case databaseLoan of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right x -> return x

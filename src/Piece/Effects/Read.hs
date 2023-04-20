module Piece.Effects.Read
  ( MonadRead,
    read,
  )
where

import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (MonadIO m, E.As err E.UserError, E.WithError err m) => String -> m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- Error.tryError $ Db.readJson datastoreLoan
  case databaseLoan of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right x -> return x

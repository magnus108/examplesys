module Piece.Effects.Change
  ( MonadChanges,
    listen,
    MonadRead,
    read,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Piece.App.Env as Env
import Piece.App.Error (AppError (..), As (..), UserError (..), WithError)
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

class Monad m => MonadChanges m where
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl ::
  ( As err UserError,
    WithError err m,
    MonadUnliftIO m,
    Env.WithLoanEnv env m
  ) =>
  String ->
  m ()
listenImpl datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  withRunInIO $ \run -> do
    R.onChange bDatabaseLoan $ \s -> do
      dataWrite <- Db.writeJson2 datastoreLoan s
      case dataWrite of
        Left _ -> run $ Error.throwError (as NotFound)
        Right y -> return y

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (MonadIO m, As err UserError, WithError err m) => String -> m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- Error.tryError $ Db.readJson datastoreLoan
  case databaseLoan of
    Left _ -> Error.throwError (as NotFound)
    Right x -> return x

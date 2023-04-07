module Piece.Effects.Change
  ( MonadChanges,
    listen,
    MonadRead,
    read,
  )
where

import Control.Monad.Fix
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

class Monad m => MonadChanges m where
  listen :: String -> m (m ())

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl :: (MonadIO m, Env.WithLoanEnv env m) => String -> m (m ())
listenImpl datastoreLoan = do
  mLoanEnv <- Has.grab @(MVar Env.LoanEnv)
  loanEnv <- readMVar mLoanEnv
  let eDatabaseLoan = Env.eDatabaseLoan loanEnv
  unregister <- liftIO $ R.register eDatabaseLoan $ Db.writeJson datastoreLoan
  return $ liftIO unregister

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (MonadIO m) => String -> m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- Db.readJson datastoreLoan
  return databaseLoan

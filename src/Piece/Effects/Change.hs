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
import System.IO.Unsafe (unsafeInterleaveIO)

class Monad m => MonadChanges m where
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl :: (MonadIO m, UI.MonadUI m, Env.WithLoanEnv env m) => String -> m ()
listenImpl datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  UI.liftUI $ UI.onChanges bDatabaseLoan $ Db.writeJson datastoreLoan

class Monad m => MonadRead m where
  read :: String -> m (Db.Database Loan.Loan)

instance MonadRead Monad.App where
  read = readImpl
  {-# INLINE read #-}

readImpl :: (MonadIO m) => String -> m (Db.Database Loan.Loan)
readImpl datastoreLoan = do
  databaseLoan <- Db.readJson datastoreLoan
  return databaseLoan

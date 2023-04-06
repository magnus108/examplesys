module Piece.Effects.Change
  ( MonadChanges,
    listen,
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
  listen :: String -> m ()

instance MonadChanges Monad.App where
  listen = listenImpl
  {-# INLINE listen #-}

listenImpl :: (MonadIO m, Env.WithLoanEnv env m) => String -> m ()
listenImpl datastoreLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let eDatabaseLoan = Env.eDatabaseLoan loanEnv
  liftIO $ R.register eDatabaseLoan $ Db.writeJson datastoreLoan

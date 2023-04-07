module Piece.Db.Loan
  ( lookup,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (MonadIO m, Env.WithLoanEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Loan.Loan))
lookup = do
  mLoanEnv <- Has.grab @(MVar Env.LoanEnv)
  loanEnv <- liftIO $ readMVar mLoanEnv
  return $ flip Db.lookup <$> Env.bDatabaseLoan loanEnv

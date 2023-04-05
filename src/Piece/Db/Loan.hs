module Piece.Db.Loan
  ( lookup,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithLoanEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Loan.Loan))
lookup = do
  loanEnv <- Has.grab @Env.LoanEnv
  return $ flip Db.lookup <$> (Env.bDatabaseLoan loanEnv)

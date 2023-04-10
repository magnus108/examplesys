module Piece.Db.Loan2
  ( lookup,
  )
where

import qualified Control.Lens.Operators as LOperators
import qualified Piece.App.Env2 as Env
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (MonadReader r m, Env.HasLoanBehavior r) => m (R.Behavior (Db.DatabaseKey -> Maybe Loan.Loan))
lookup = do
  r <- ask
  let bDatabaseLoan = r LOperators.^. Env.loanBehavior . Env.bDatabaseLoan
  return $ flip Db.lookup <$> bDatabaseLoan

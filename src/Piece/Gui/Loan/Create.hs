{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tDatabaseLoan,
    tLoanFilter,
    Create,
  )
where

import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Graphics.UI.Threepenny.Core ((#), (#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Loan as DbLoan
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data Create = Create
  { view :: UI.Element,
    tDatabaseLoan :: R.Tidings (Db.Database Loan.Loan),
    tLoanSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tLoanFilter :: R.Tidings String
  }

instance UI.Widget Create where
  getElement = view

showLoan :: (Env.WithLoanEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showLoan = do
  bLookup <- DbLoan.lookup
  return $ (maybe "" Loan.name .) <$> bLookup

displayLoan :: (Env.WithLoanEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayLoan = do
  show <- showLoan
  return $ (UI.string .) <$> show

setup :: (UI.MonadUI m, Env.WithLoanEnv env m, MonadFix m) => UI.Window -> m Create
setup window = mdo
  listBoxLoan <- UI.liftUI $ Widgets.listBox bListBoxLoans (Env.bSelectionLoan loanEnv) bDisplayLoan
  filterLoan <- UI.liftUI $ Widgets.entry (Env.bFilterLoan loanEnv)
  view <- UI.liftUI $ Elements.div #+ [UI.element filterLoan, UI.element listBoxLoan]

  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = Widgets.userText filterLoan
      tFilterLoan = isPrefixOf <$> tLoanFilter
      bFilterLoan = R.facts tFilterLoan

  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  bShowLoan <- showLoan
  bDisplayLoan <- displayLoan

  let bListBoxLoans :: R.Behavior [Db.DatabaseKey]
      bListBoxLoans =
        (\p display -> filter (p . display) . Db.keys)
          <$> bFilterLoan
          <*> bShowLoan
          <*> bDatabaseLoan

  let tDatabaseLoan = R.tidings bDatabaseLoan $ Unsafe.head <$> R.unions []

  return Create {..}
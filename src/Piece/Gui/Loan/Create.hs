{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tDatabaseLoan,
    tLoanFilter,
    eCreate,
    Create,
  )
where

import Control.Monad.Fix
import Graphics.UI.Threepenny.Core ((#), (#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Loan as DbLoan
import Reactive.Threepenny ((<@))
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data Create = Create
  { view :: UI.Element,
    tDatabaseLoan :: R.Tidings (Db.Database Loan.Loan),
    tLoanSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tLoanFilter :: R.Tidings String,
    eCreate :: R.Event ()
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

bListBox :: (Env.WithLoanEnv env m) => UI.Behavior (String -> Bool) -> m (R.Behavior [Db.DatabaseKey])
bListBox bFilterLoan = do
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  bShowLoan <- showLoan
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterLoan
      <*> bShowLoan
      <*> bDatabaseLoan

setup :: (UI.MonadUI m, Env.WithLoanEnv env m, MonadFix m) => m Create
setup = mdo
  listBoxLoan <- UI.liftUI $ Widgets.listBox bListBoxLoans (Env.bSelectionLoan loanEnv) bDisplayLoan
  filterLoan <- UI.liftUI $ Widgets.entry (Env.bFilterLoan loanEnv)

  bob <- UI.liftUI $ UI.string "bob"
  btn <- UI.liftUI $ Elements.button # UI.set UI.children [bob]
  view <- UI.liftUI $ Elements.div # UI.set UI.children [UI.getElement listBoxLoan, UI.getElement filterLoan, btn]

  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = Widgets.userText filterLoan
      tFilterLoan = isPrefixOf <$> tLoanFilter
      bFilterLoan = R.facts tFilterLoan

  let eCreate = Events.click btn

  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  bDisplayLoan <- displayLoan

  bListBoxLoans <- bListBox bFilterLoan

  let tDatabaseLoan = R.tidings bDatabaseLoan $ Unsafe.head <$> R.unions [Db.create (Loan.Loan "dadda") <$> bDatabaseLoan <@ eCreate]
  return Create {..}

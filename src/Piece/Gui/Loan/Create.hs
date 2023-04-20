{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tDatabaseLoan,
    tLoanFilter,
    eCreate,
    Create,
  )
where

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

setup :: Env.Env -> UI.UI Create
setup env = mdo
  listBoxLoan <- Widgets.listBox bListBoxLoans (loanBehavior LOperators.^. Env.bSelectionLoan) bDisplayLoan
  filterLoan <- Widgets.entry (loanBehavior LOperators.^. Env.bFilterLoan)
  bob <- UI.string "bob"
  btn <- Elements.button UI.# UI.set UI.children [bob]
  view <- Elements.div UI.# UI.set UI.children [UI.getElement listBoxLoan, UI.getElement filterLoan, btn]

  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = Widgets.userText filterLoan
      tFilterLoan = isPrefixOf <$> tLoanFilter
      bFilterLoan = R.facts tFilterLoan

  let eCreate = Events.click btn

  r <- ask
  let loanBehavior = r LOperators.^. Env.loanBehavior
  let bDatabaseLoan = loanBehavior LOperators.^. Env.bDatabaseLoan

  bDisplayLoan <- displayLoan
  bListBoxLoans <- bListBox bFilterLoan

  let tDatabaseLoan =
        R.tidings bDatabaseLoan $
          Unsafe.head <$> R.unions [Db.create (Loan.Loan "dadda") <$> bDatabaseLoan R.<@ eCreate]

  return Create {..}

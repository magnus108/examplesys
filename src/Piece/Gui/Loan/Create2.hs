{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create2
  ( Create,
    setup,
    tDatabaseLoan,
    tLoanFilter,
    eCreate,
  )
where

import qualified Control.Lens.Operators as LOperators
import qualified Control.Monad.Fix as MFix
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env2 as Env
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Loan2 as LoanDb
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

showLoan :: (MonadReader r m, Env.HasLoanBehavior r) => m (R.Behavior (Db.DatabaseKey -> String))
showLoan = do
  bLookup <- LoanDb.lookup
  return $ (maybe "" Loan.name .) <$> bLookup

displayLoan :: (MonadReader r m, Env.HasLoanBehavior r) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayLoan = do
  show <- showLoan
  return $ (UI.string .) <$> show

bListBox :: (MonadReader r m, Env.HasLoanBehavior r) => UI.Behavior (String -> Bool) -> m (R.Behavior [Db.DatabaseKey])
bListBox bFilterLoan = do
  r <- ask
  let bDatabaseLoan = r LOperators.^. Env.loanBehavior . Env.bDatabaseLoan
  bShowLoan <- showLoan
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterLoan
      <*> bShowLoan
      <*> bDatabaseLoan

setup :: (UI.MonadUI m, MonadReader r m, Env.HasLoanBehavior r, MFix.MonadFix m) => m Create
setup = mdo
  listBoxLoan <- UI.liftUI $ Widgets.listBox bListBoxLoans (loanBehavior LOperators.^. Env.bSelectionLoan) bDisplayLoan
  filterLoan <- UI.liftUI $ Widgets.entry (loanBehavior LOperators.^. Env.bFilterLoan)

  bob <- UI.liftUI $ UI.string "bob"
  btn <- UI.liftUI $ Elements.button UI.# UI.set UI.children [bob]
  view <- UI.liftUI $ Elements.div UI.# UI.set UI.children [UI.getElement listBoxLoan, UI.getElement filterLoan, btn]

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

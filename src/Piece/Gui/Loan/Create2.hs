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
  show' <- showLoan
  return $ (UI.string .) <$> show'

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

canIDoWithMVar :: (MonadIO m) => UI.Window -> UI.UI a -> m a
canIDoWithMVar window ui = do
  mvar <- newEmptyMVar
  s <- liftIO $ UI.runUI window $ do
    traceShowM "ggg2"
    res <- ui
    traceShowM "ggg"
    UI.liftIOLater $ do
      traceShowM "ggg11123"
      putMVar mvar res
    traceShowM "ggg3"
  readMVar mvar

setup :: (MonadReader r m, MonadIO m, Env.HasLoanBehavior r, MFix.MonadFix m) => UI.Window -> m Create
setup window = mdo
  listBoxLoan <- canIDoWithMVar window $ Widgets.listBox bListBoxLoans (loanBehavior LOperators.^. Env.bSelectionLoan) bDisplayLoan
  filterLoan <- canIDoWithMVar window $ Widgets.entry (loanBehavior LOperators.^. Env.bFilterLoan)

  bob <- canIDoWithMVar window $ UI.string "bob"
  btn <- canIDoWithMVar window $ Elements.button UI.# UI.set UI.children [bob]
  view <- canIDoWithMVar window $ Elements.div UI.# UI.set UI.children [UI.getElement listBoxLoan, UI.getElement filterLoan, btn]

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

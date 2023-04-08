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
import qualified Graphics.UI.Threepenny.Events as Events
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

setup :: (UI.MonadUI m, Env.WithLoanEnv env m, MonadFix m) => UI.Window -> m Create
setup window = mdo
  listBoxLoan <- UI.liftUI $ Widgets.listBox bListBoxLoans (Env.bSelectionLoan loanEnv) bDisplayLoan
  filterLoan <- entry (Env.bFilterLoan loanEnv)
  bob <- UI.liftUI $ UI.string "bob"
  view <- UI.liftUI $ Elements.div # UI.set UI.children [bob, UI.getElement listBoxLoan, UI.getElement filterLoan]

  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = userText filterLoan
      tFilterLoan = isPrefixOf <$> tLoanFilter
      bFilterLoan = R.facts tFilterLoan

  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  bShowLoan <- showLoan
  bDisplayLoan <- displayLoan

  bListBoxLoans <- bListBox bFilterLoan

  let tDatabaseLoan = R.tidings bDatabaseLoan $ Unsafe.head <$> R.unions []
  traceShowM "WTF"
  return Create {..}

data TextEntry = TextEntry
  { _elementTE :: UI.Element,
    _userTE :: UI.Tidings String
  }

instance UI.Widget TextEntry where getElement = _elementTE

userText :: TextEntry -> UI.Tidings String
userText = _userTE

entry :: (UI.MonadUI m, Env.WithLoanEnv env m, MonadFix m) => UI.Behavior String -> m TextEntry
entry bValue = do
  -- single text entry
  input <- UI.liftUI $ Elements.input

  bEditing <- UI.liftUI $ UI.stepper False $ and <$> R.unions [True <$ Events.focus input, False <$ Events.blur input]

  -- BLiver måske aldirg kaldt nu?
  UI.liftUI $ UI.onChanges bValue $ \s -> do
    editing <- UI.currentValue bEditing
    when (not editing) $ void $ UI.element input # UI.set UI.value s

  -- liftIOLater $ onChange bValue $ \s -> runUI window $ do
  ---   editing <- liftIO $ currentValue bEditing
  -- when (not editing) $ void $ element input # set value s

  let _elementTE = input
      _userTE = UI.tidings bValue $ Events.valueChange input
  return TextEntry {..}

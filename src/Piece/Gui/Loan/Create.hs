{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tDatabaseLoan,
    tLoanFilter,
    eCreate,
    Create,
  )
where

import Control.Concurrent (Chan, writeChan)
import Control.Monad.Fix
import GHC.IO (unsafeInterleaveIO)
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

setup :: (Env.WithLoanEnv env m, MonadIO m, MonadFix m) => Chan (IO ()) -> UI.Window -> m Create
setup chan window = mdo
  traceShowM "32"
  -- listBoxLoan <- runM chan window $ Widgets.listBox bListBoxLoans (Env.bSelectionLoan loanEnv) bDisplayLoan
  traceShowM "12"
  filterLoan <- entry chan window (Env.bFilterLoan loanEnv)
  traceShowM "121"
  bob <- runM chan window $ UI.string "bob"
  traceShowM "122"
  btn <- runM chan window $ Elements.button # UI.set UI.children [bob]
  traceShowM "123"
  view <- runM chan window $ Elements.div # UI.set UI.children [UI.getElement filterLoan, btn]
  traceShowM "124"

  --  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = userText filterLoan
      tFilterLoan = isPrefixOf <$> tLoanFilter
      bFilterLoan = R.facts tFilterLoan

  let eCreate = Events.click btn

  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  bDisplayLoan <- displayLoan

  bListBoxLoans <- bListBox bFilterLoan

  let tDatabaseLoan = R.tidings bDatabaseLoan $ Unsafe.head <$> R.unions [Db.create (Loan.Loan "dadda") <$> bDatabaseLoan <@ eCreate]
  return Create {..}

runM :: (MonadIO m, MonadFix m) => Chan (IO ()) -> UI.Window -> UI.UI a -> m a
runM chan window ui = do
  i <- liftIO $ UI.runUI window ui
  liftIO $ writeChan chan $ do
    -- i <- liftIO $ UI.runUI window ui
    return ()
  return i

data TextEntry = TextEntry
  { _elementTE :: UI.Element,
    _userTE :: UI.Tidings String
  }

instance UI.Widget TextEntry where getElement = _elementTE

userText :: TextEntry -> UI.Tidings String
userText = _userTE

onChanges :: Chan (IO ()) -> UI.Window -> UI.Behavior a -> (a -> UI.UI void) -> IO ()
onChanges chan win b f = do
  traceShowM "a2a"
  R.onChange
    b
    ( \x -> void $ do
        traceShowM "a2aaa"
        runM chan win $ f x
    )

entry :: (Env.WithLoanEnv env m, MonadIO m, MonadFix m) => Chan (IO ()) -> UI.Window -> UI.Behavior String -> m TextEntry
entry chan window bValue = do
  traceShowM "c2"
  input <- runM chan window $ Elements.input
  traceShowM "b2"
  bEditing <- runM chan window $ UI.stepper False $ and <$> R.unions [True <$ Events.focus input, False <$ Events.blur input]
  traceShowM "a2"

  -- BLiver måske aldirg kaldt nu?
  x <- liftIO $ onChanges chan window bValue $ \s -> do
    traceShowM "gg"
  --    editing <- UI.currentValue bEditing
  --   when (not editing) $ void $ return () -- UI.element input # UI.set UI.value s
  traceShowM "a22"
  let _elementTE = input
      _userTE = UI.tidings bValue $ Events.valueChange input
  return TextEntry {..}

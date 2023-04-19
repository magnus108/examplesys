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
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Attributes as UI
import Graphics.UI.Threepenny.Core ((#), (#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Monad as CakeSlayer
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

setup :: (CakeSlayer.MonadUnliftUI m, Env.WithLoanEnv env m, MonadFix m) => UI.Window -> m Create
setup window = mdo
  loanEnv2 <- Has.grab @Env.LoanEnv
  bDisplayLoan2 <- displayLoan
  traceShowM "fucks"
  x <- CakeSlayer.withRunInUI window $ \x -> listBox (pure []) (pure (Just 1)) bDisplayLoan2
  traceShowM "fuck"
  return Create {..}

data ListBox a = ListBox
  { _elementLB :: UI.Element,
    _selectionLB :: UI.Tidings (Maybe a)
  }

instance UI.Widget (ListBox a) where getElement = _elementLB

userSelection :: ListBox a -> UI.Tidings (Maybe a)
userSelection = _selectionLB

-- | Create a 'ListBox'.
listBox ::
  forall a.
  Ord a =>
  -- | list of items
  UI.Behavior [a] ->
  -- | selected item
  UI.Behavior (Maybe a) ->
  -- | display for an item
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (ListBox a)
listBox bitems bsel bdisplay = do
  traceShowM "gg3"
  list <- Elements.select
  traceShowM "gg33"

  -- animate output items
  traceShowM "gg36"
  UI.element list UI.# UI.sink items (map <$> bdisplay <*> bitems)
  traceShowM "gg34"

  -- animate output selection
  let bindices :: UI.Behavior (Map a Int)
      bindices = (Map.fromList . flip zip [0 ..]) <$> bitems
      bindex = lookupIndex <$> bindices <*> bsel

      lookupIndex indices Nothing = Nothing
      lookupIndex indices (Just sel) = Map.lookup sel indices

  traceShowM "gg32"
  UI.element list # UI.sink UI.selection bindex

  traceShowM "gg31"
  -- changing the display won't change the current selection
  -- eDisplay <- changes display
  -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

  -- user selection
  let bindices2 :: UI.Behavior (Map.Map Int a)
      bindices2 = Map.fromList . zip [0 ..] <$> bitems

      _selectionLB =
        UI.tidings bsel $
          lookupIndex UI.<$> bindices2 UI.<@> UI.selectionChange list
      _elementLB = list

  traceShowM "gg36"
  return ListBox {..}

items = UI.mkWriteAttr $ \i x -> void $ do
  return x # UI.set UI.children [] #+ map (\i -> Elements.option #+ [i]) i

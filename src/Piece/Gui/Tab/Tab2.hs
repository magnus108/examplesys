{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab2
  ( setup,
    Create (..),
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Tab.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data Create = Create
  { view :: UI.Element
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  tabEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.TabEnv

  bListBoxTabs <- liftIO $ Monad.runApp env Behavior.bListBox
  let bSelection = Env.bSelectionTab tabEnv

  (e, h) <- liftIO R.newEvent
  (eSelect, hSelect) <- liftIO R.newEvent

  window <- UI.askWindow
  traceShowM "hemm"
  UI.liftIOLater $ R.onChange bListBoxTabs $ \xs -> do
    traceShowM "hemmmm"
    let mkElem k = UI.runUI window $ do
          btn <- UI.button UI.#+ [UI.string (show k)]
          UI.on UI.click btn $ \_ -> liftIO $ hSelect k
          return btn

    newElements <- mapM mkElem xs

    h newElements

  elements <- R.stepper [] $ Unsafe.head <$> R.unions [e]
  view <- UI.div UI.# UI.sink UI.children elements

  return Create {..}

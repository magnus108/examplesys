{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab
  ( setup,
    Create (..),
    userSelection,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Tab.Behavior as Behavior
import qualified Reactive.Threepenny as R

data Create = Create
  { view :: UI.Element,
    eTabSelection :: R.Event (Maybe Db.DatabaseKey)
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  boxTab <- tabBox bListBoxTabs bDisplayButtonTabHandler
  view <- UI.div UI.# UI.set UI.children [UI.getElement boxTab]

  let eTabSelection = userSelection boxTab

  bDisplayButtonTabHandler <- liftIO $ Monad.runApp env Behavior.displayButtonTabHandler
  bListBoxTabs <- liftIO $ Monad.runApp env Behavior.bListBox

  bDisplayViewTab <- liftIO $ Monad.runApp env $ Behavior.displayViewTab

  return Create {..}

data TabBox a = TabBox
  { _elementTB :: UI.Element,
    _selectionTB :: R.Event (Maybe a)
  }

instance UI.Widget (TabBox a) where
  getElement = _elementTB

userSelection :: TabBox a -> R.Event (Maybe a)
userSelection = _selectionTB

tabBox ::
  UI.Behavior [a] ->
  UI.Behavior ((a -> UI.UI ()) -> a -> UI.UI UI.Element) ->
  UI.UI (TabBox a)
tabBox bitems bdisplay = mdo
  content <- UI.div UI.#. "navbar-start"
  nav <-
    UI.mkElement "nav"
      UI.#. "navbar is-dark"
      UI.#+ [ UI.div
                UI.#. "navbar-menu"
                UI.#+ [UI.element content]
            ]

  (e, h) <- liftIO UI.newEvent
  _ <- return content UI.# UI.sink items ((fmap .) <$> bdisplay <*> pure (liftIO . h . Just) <*> bitems)

  let _selectionTB = e
      _elementTB = nav

  return TabBox {..}

items :: UI.WriteAttr UI.Element [UI.UI UI.Element]
items = UI.mkWriteAttr $ \i x -> void $ do
  return x UI.# UI.set UI.children [] UI.#+ i

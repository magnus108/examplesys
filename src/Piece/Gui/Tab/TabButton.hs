{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.TabButton
  ( setup,
    userSelection,
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
import qualified Piece.Db.Tab as Tab
import qualified Piece.Gui.Tab.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data Create a = Create
  { view :: UI.Element,
    _selectionTB :: R.Tidings (Maybe a)
  }

instance UI.Widget (Create a) where
  getElement = view

userSelection :: (Create a) -> R.Tidings (Maybe a)
userSelection = _selectionTB

setup :: Monad.AppEnv -> UI.UI (Create Db.DatabaseKey)
setup env = mdo
  tabEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.TabEnv

  bDisplay <- liftIO $ Monad.runApp env Behavior.displayTab
  bListBoxTabs <- liftIO $ Monad.runApp env Tab.listBox
  let bSelection = Env.bSelectionTab tabEnv

  _listBox <- listBox UI.# UI.sink items ((,,) <$> bListBoxTabs <*> bSelection <*> bDisplay)

  let _eSelection = _e _listBox
      _selectionTB = R.tidings bSelection _eSelection
      view = UI.getElement _listBox

  return Create {..}

data ListBox a = ListBox
  { _view :: UI.Element,
    _menu :: UI.Element,
    _e :: R.Event (Maybe a),
    _h :: R.Handler (Maybe a)
  }

instance UI.Widget (ListBox a) where
  getElement = _view

listBox :: UI.UI (ListBox a)
listBox = do
  (_e, _h) <- liftIO R.newEvent
  _menu <- UI.div UI.#. "navbar-start"
  _view <-
    UI.mkElement "nav"
      UI.#. "navbar is-dark"
      UI.#+ [ UI.div
                UI.#. "navbar-menu"
                UI.#+ [UI.element _menu]
            ]
  return ListBox {..}

items :: Eq a => UI.WriteAttr (ListBox a) ([a], Maybe a, a -> UI.UI UI.Element)
items = UI.mkWriteAttr $ \(ix, v, f) x -> void $ do
  return (_menu x)
    UI.# UI.set UI.children []
    UI.#+ fmap
      ( \i -> do
          btn <-
            UI.a
              UI.#. (if Just i == v then "navbar-item is-active" else "navbar-item")
              UI.#+ [f i]
          UI.on UI.click btn $ \_ -> liftIO $ (_h x) (Just i)
          return btn
      )
      ix

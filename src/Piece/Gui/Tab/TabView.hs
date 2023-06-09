{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.TabView
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

setup :: Monad.AppEnv -> [UI.Element] -> UI.UI Create
setup env tabs = mdo
  view <- UI.div UI.# UI.sink items (fromMaybe (UI.string "not found") <$> bDisplayViewTab')
  tabEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.TabEnv
  bDisplayViewTab <- liftIO $ Monad.runApp env $ Behavior.displayViewTab tabs
  let bDisplayViewTab' = (\f y -> y >>= (\z -> ((\x -> UI.element x) <$> (f z)))) <$> bDisplayViewTab <*> bSelection

  let bSelection = Env.bSelectionTab tabEnv

  return Create {..}

items :: UI.WriteAttr UI.Element (UI.UI UI.Element)
items = UI.mkWriteAttr $ \i x -> void $ do
  return x
    UI.# UI.set UI.children []
    UI.#+ [i]

{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab
  ( setup,
    Create,
  )
where

import Control.Comonad
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.ListZipper as LZ
import qualified Piece.Db.ListZipper as ListZipper
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
  zipperBoxTab <- zipperBox bListBoxTabs bDisplayTab
  view <- UI.div UI.# UI.set UI.children [UI.getElement zipperBoxTab]

  let bFilterTab = pure (const True)
  tabEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.TabEnv
  bDisplayTab <- liftIO $ Monad.runApp env $ Behavior.displayTab

  bListBoxTabs <- liftIO $ Monad.runApp env $ Behavior.bZipperBox bFilterTab

  return Create {..}

data ZipperBox a = ZipperBox
  { _elementZB :: UI.Element
  }

instance UI.Widget (ZipperBox a) where getElement = _elementZB

zipperBox ::
  forall a.
  Ord a =>
  UI.Behavior [a] ->
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (ZipperBox a)
zipperBox bitems bdisplay = mdo
  (eClick, hClick) <- liftIO R.newEvent

  start <- UI.div UI.#. "navbar-start"
  nav <-
    UI.mkElement "nav"
      UI.#. "navbar is-dark"
      UI.#+ [ UI.div
                UI.#. "navbar-menu"
                UI.#+ [UI.element start]
            ]

  bZipperBoxTabs <- R.stepper (Unsafe.fromJust $ LZ.fromList [0 ..]) $ Unsafe.head <$> R.unions [eClick]

  let bContent = fmap <$> bdisplay <*> bitems
      bButtons = LZ.toList . mkButton hClick <$> bZipperBoxTabs

  UI.element start UI.# UI.sink items (zip <$> bButtons <*> bContent)

  let _elementZB = nav

  return ZipperBox {..}

mkButton :: R.Handler (LZ.ListZipper Int) -> LZ.ListZipper Int -> LZ.ListZipper (UI.UI UI.Element)
mkButton hClick lz =
  extend
    ( \wa -> do
        button <- UI.a
        UI.on UI.click button $ \_ -> void $ do
          liftIO $ hClick wa
        return button
    )
    lz
    & LZ.mapFocus (UI.#. "navbar-item is-active")
    & LZ.mapLefts (UI.#. "navbar-item")
    & LZ.mapRights (UI.#. "navbar-item")

items :: UI.WriteAttr UI.Element [(UI.UI UI.Element, UI.UI UI.Element)]
items = UI.mkWriteAttr $ \i x -> void $ do
  return x UI.# UI.set UI.children [] UI.#+ map (\(button, content) -> button UI.#+ [content]) i

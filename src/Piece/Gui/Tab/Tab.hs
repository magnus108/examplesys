{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab
  ( setup,
    tListZipperTab,
    tTabFilter,
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
  { view :: UI.Element,
    tListZipperTab :: R.Tidings (ListZipper.ListZipper Tab.Tab),
    tTabFilter :: R.Tidings Bool
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  listBoxTab <- zipperBox bListBoxTabs bDisplayTab
  view <- UI.div UI.# UI.set UI.children [UI.getElement listBoxTab]

  let bFilterTab = pure (const True)
  tabEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  bDisplayTab <- liftIO $ Monad.runApp env $ Behavior.displayTab
  bListBoxTabs <- liftIO $ Monad.runApp env $ Behavior.bListBox bFilterTab

  let tDatabaseLoan =
        R.tidings bDatabaseTab $
          Unsafe.head
            <$> R.unions
              []

  return Create {..}

data ZipperBox a = ZipperBox
  { _elementZB :: UI.Element,
    _selectionZB :: R.Tidings (LZ.ListZipper a)
  }

instance UI.Widget (ZipperBox a) where getElement = _elementZB

userSelection :: (ZipperBox a) -> R.Tidings (LZ.ListZipper a)
userSelection = _selectionZB

zipperBox ::
  UI.Behavior (LZ.ListZipper a) ->
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (ZipperBox a)
zipperBox bitems bdisplay = do
  nav <- navBox bdisplay

  UI.sink items bitems (return nav)

  --  UI.element nav UI.# UI.sink UI.selection bindex

  let _selectionZB =
        UI.tidings bitems (_click nav)
      _elementZB = (UI.getElement nav)

  return ZipperBox {..}

data NavBox a = NavBox
  { _elementNav :: UI.Element,
    _handler :: R.Handler (LZ.ListZipper a),
    _click :: R.Event (LZ.ListZipper a),
    _display :: R.Behavior (a -> UI.UI UI.Element)
  }

instance UI.Widget (NavBox a) where getElement = _elementNav

navBox :: UI.Behavior (a -> UI.UI UI.Element) -> UI.UI (NavBox a)
navBox bdisplay = do
  (eClick, hClick) <- liftIO R.newEvent
  nav <- UI.div
  let _elementNav = nav
  return NavBox {..}

items :: UI.WriteAttr (NavBox a) (LZ.ListZipper a)
items = items' _handler _display

items' :: (NavBox a -> R.Handler (LZ.ListZipper a)) -> R.Behavior (a -> UI.UI UI.Element) -> UI.WriteAttr (NavBox a) (LZ.ListZipper a)
items' f d = UI.mkWriteAttr $ \i x -> void $ do
  UI.element x
    UI.# UI.set UI.children []
    UI.#+ LZ.toList
      ( extend
          ( \wa -> do
              button <- UI.button UI.#+ [extract wa]
              UI.on UI.click button $ \_ -> void $ do
                liftIO $ f x wa
              return button
          )
          i
      )

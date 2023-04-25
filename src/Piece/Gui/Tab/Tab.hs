{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab
  ( setup,
    Create,
  )
where

import Control.Comonad
import Data.List.Index (imap)
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
  zipperBoxTab <- zipperBox bListBoxTabs bDisplayButtonTab bDisplayViewTab
  zipperBoxTab2 <- zipperBox2 bListBoxTabs (lol) bDisplayButtonTab
  view <- UI.div UI.# UI.set UI.children [UI.getElement zipperBoxTab, UI.getElement zipperBoxTab2]

  let g = userSelection2 zipperBoxTab2
      e = UI.rumors g
  lol <- UI.stepper Nothing $ Unsafe.head <$> R.unions [e]

  let bFilterTab = pure (const True)
  bDisplayButtonTab <- liftIO $ Monad.runApp env $ Behavior.displayButtonTab
  bDisplayViewTab <- liftIO $ Monad.runApp env $ Behavior.displayViewTab
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
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (ZipperBox a)
zipperBox bitems bdisplay bdisplay2 = mdo
  (eClick, hClick) <- liftIO R.newEvent

  content <- UI.div
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
  let bContent2 = fmap <$> bdisplay2 <*> bitems
      bButtons = LZ.toList . mkButton hClick <$> bZipperBoxTabs
      bButtons2 = extract <$> bZipperBoxTabs

  UI.element start UI.# UI.sink items (zip <$> bButtons <*> bContent)
  UI.element content UI.# UI.sink items2 (Unsafe.at <$> bButtons2 <*> bContent2)

  x <- UI.div UI.#+ [UI.element nav, UI.element content]
  let _elementZB = x

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

items2 :: UI.WriteAttr UI.Element (UI.UI UI.Element)
items2 = UI.mkWriteAttr $ \i x -> void $ do
  return x UI.# UI.set UI.children [] UI.#+ [i]

data ZipperBox2 a = ZipperBox2
  { _elementZB2 :: UI.Element,
    _selectionZB2 :: R.Tidings (Maybe a)
  }

instance UI.Widget (ZipperBox2 a) where
  getElement = _elementZB2

userSelection2 :: ZipperBox2 a -> R.Tidings (Maybe a)
userSelection2 = _selectionZB2

zipperBox2 ::
  forall a.
  Ord a =>
  UI.Behavior [a] ->
  UI.Behavior (Maybe a) ->
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (ZipperBox2 a)
zipperBox2 bitems bsel bdisplay = mdo
  view <- viewBox

  return view UI.# UI.sink items' (fmap <$> bdisplay <*> bitems)

  let bindices :: UI.Behavior (Map.Map a Int)
      bindices = (Map.fromList . flip zip [0 ..]) <$> bitems
      bindex = lookupIndex <$> bindices <*> bsel

      lookupIndex indices Nothing = Nothing
      lookupIndex indices (Just sel) = Map.lookup sel indices

  return view UI.# UI.sink selection' bindex

  let bindices2 :: UI.Behavior (Map.Map Int a)
      bindices2 = Map.fromList . zip [0 ..] <$> bitems

      _selectionZB2 =
        R.tidings bsel $
          lookupIndex <$> bindices2 UI.<@> selectionChange view
      _elementZB2 = UI.getElement view

  return ZipperBox2 {..}

data ViewBox = ViewBox
  { _content :: UI.Element,
    selectionChange :: R.Event (Maybe Int),
    _handler :: R.Handler (Maybe Int),
    _buttons :: TVar [UI.UI UI.Element]
  }

instance UI.Widget ViewBox where
  getElement = _content

viewBox :: UI.UI ViewBox
viewBox = mdo
  _buttons <- liftIO $ newTVarIO []
  (eClick, hClick) <- liftIO R.newEvent
  _content <- UI.div

  let selectionChange = eClick
      _handler = hClick

  return ViewBox {..}

-- FLYT UD I normal view og så attach handler senere.
items' :: UI.Attr ViewBox [UI.UI UI.Element]
items' = UI.mkReadWriteAttr (readTVarIO . _buttons) $ \i x -> void $ do
  let btns =
        imap
          ( \i y -> do
              btn <- UI.button UI.#. "navbar-item" UI.#+ [y]
              UI.on UI.click btn $ \_ -> void $ do
                liftIO $ _handler x (Just i)
              return btn
          )
          i
  liftIO $ atomically $ writeTVar (_buttons x) btns
  UI.element x
    UI.# UI.set UI.children []
    UI.#+ btns

items'' :: UI.WriteAttr ViewBox [UI.UI UI.Element]
items'' = UI.mkWriteAttr $ \i x -> void $ do
  UI.element x UI.# UI.set UI.children [] UI.#+ i

selection' :: UI.WriteAttr ViewBox (Maybe Int)
selection' = UI.mkWriteAttr $ \i x -> void $ do
  mi <- UI.get items' x
  case i of
    Nothing -> return ()
    Just y -> void $ do
      new <- Unsafe.at (y) mi UI.#. "navbar-item is-active"
      UI.set items'' ((take (y) mi) ++ [return new] ++ (drop (y + 1) mi)) (return x)

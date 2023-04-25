{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab
  ( setup,
    Create,
  )
where

import Data.List.Index (iforM, imap)
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Piece.App.Monad as Monad
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
  boxTab <- tabBox bListBoxTabs bSelection bDisplayButtonTab
  view <- UI.div UI.# UI.set UI.children [UI.getElement boxTab]

  let tSelection = userSelection boxTab
      eSelection = UI.rumors tSelection

  bSelection <- UI.stepper Nothing $ Unsafe.head <$> R.unions [eSelection]

  let bFilterTab = pure (const True)
  bDisplayButtonTab <- liftIO $ Monad.runApp env $ Behavior.displayButtonTab
  bListBoxTabs <- liftIO $ Monad.runApp env $ Behavior.bListBox bFilterTab
  bDisplayViewTab <- liftIO $ Monad.runApp env $ Behavior.displayViewTab

  return Create {..}

data TabBox a = TabBox
  { _elementTB :: UI.Element,
    _selectionTB :: R.Tidings (Maybe a)
  }

instance UI.Widget (TabBox a) where
  getElement = _elementTB

userSelection :: TabBox a -> R.Tidings (Maybe a)
userSelection = _selectionTB

tabBox ::
  forall a.
  Ord a =>
  UI.Behavior [a] ->
  UI.Behavior (Maybe a) ->
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (TabBox a)
tabBox bitems bsel bdisplay = mdo
  view <- viewBox

  _ <- return view UI.# UI.sink items (fmap <$> bdisplay <*> bitems)

  let bindices :: UI.Behavior (Map.Map a Int)
      bindices = (Map.fromList . flip zip [0 ..]) <$> bitems
      bindex = lookupIndex <$> bindices <*> bsel

      lookupIndex indices Nothing = Nothing
      lookupIndex indices (Just sel) = Map.lookup sel indices

  _ <- return view UI.# UI.sink selection bindex

  let bindices2 :: UI.Behavior (Map.Map Int a)
      bindices2 = Map.fromList . zip [0 ..] <$> bitems

      _selectionTB =
        R.tidings bsel $
          lookupIndex <$> bindices2 UI.<@> _eClick view
      _elementTB = UI.getElement view

  return TabBox {..}

data ViewBox = ViewBox
  { _content :: UI.Element,
    _nav :: UI.Element,
    _hButtons :: UI.Handler [UI.UI UI.Element],
    _hSelection :: UI.Handler (Maybe Int),
    _eClick :: R.Event (Maybe Int),
    _hClick :: R.Handler (Maybe Int)
  }

instance UI.Widget ViewBox where
  getElement = _nav

viewBox :: UI.UI ViewBox
viewBox = mdo
  (_eClick, _hClick) <- liftIO R.newEvent
  (_eSelection, _hSelection) <- liftIO R.newEvent
  (_eButtons, _hButtons) <- liftIO R.newEvent

  _bButtons <- UI.stepper [] $ Unsafe.head <$> R.unions [_eButtons]
  _bSelection <- UI.stepper Nothing $ Unsafe.head <$> R.unions [_eSelection]

  let buttons =
        ( \xs y ->
            imap
              ( \index x -> do
                  case y of
                    Nothing -> x
                    Just y' ->
                      if (y' == index)
                        then x UI.#. "navbar-item is-active"
                        else x
              )
              xs
        )
          <$> _bButtons
          <*> _bSelection

  _content <- UI.div UI.#. "navbar-start" UI.# UI.sink items' buttons

  _nav <-
    UI.mkElement "nav"
      UI.#. "navbar is-dark"
      UI.#+ [ UI.div
                UI.#. "navbar-menu"
                UI.#+ [UI.element _content]
            ]

  return ViewBox {..}

items :: UI.WriteAttr ViewBox [UI.UI UI.Element]
items = UI.mkWriteAttr $ \i x -> void $ do
  liftIO $
    _hButtons x $
      imap
        ( \index btn -> do
            btn' <- btn
            UI.on UI.click btn' $ \_ -> void $ liftIO $ do
              _hClick x (Just index)
            return btn'
        )
        i

selection :: UI.WriteAttr ViewBox (Maybe Int)
selection = UI.mkWriteAttr $ \i x -> void $ do
  liftIO $ _hSelection x i

items' = UI.mkWriteAttr $ \i x -> void $ do
  return x UI.# UI.set UI.children [] UI.#+ i

-- buttons <- liftIO $ readTVarIO $ _buttons x
-- case i of
-- Nothing -> return ()
-- Just index -> void $ do
-- new <- Unsafe.at index buttons UI.#. "navbar-item is-active"
-- UI.set items'' ((take index buttons ) ++ [return new] ++ (drop (index + 1) buttons)) (return x)
--
-- items'' :: UI.WriteAttr ViewBox [UI.UI UI.Element]
-- items'' = UI.mkWriteAttr $ \i x -> void $ do
-- return (_content x) UI.# UI.set UI.children [] UI.#+ i

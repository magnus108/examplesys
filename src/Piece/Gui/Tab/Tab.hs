{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Tab.Tab
  ( setup,
    tListZipperTab,
    tTabFilter,
    Create,
  )
where

import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
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
  { _elementLB :: UI.Element
  -- , _selectionLB :: Tidings (Maybe a)
  }

instance UI.Widget (ZipperBox a) where getElement = _elementLB

-- | User changes to the current selection (possibly empty).
-- userSelection :: ListBox a -> Tidings (Maybe a)
-- userSelection = _selectionLB

-- | Create a 'ListBox'.
zipperBox ::
  forall a.
  Ord a =>
  UI.Behavior [a] ->
  -- | display for an item
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.UI (ZipperBox a)
zipperBox bitems bdisplay = do
  list <- UI.select

  -- animate output items
  UI.element list UI.# UI.sink items (map <$> bdisplay <*> bitems)

  -- animate output selection
  let bindices :: UI.Behavior (Map.Map a Int)
      bindices = (Map.fromList . flip zip [0 ..]) <$> bitems
      bindex = undefined -- lookupIndex <$> bindices <*> bsel
      lookupIndex indices Nothing = Nothing
      lookupIndex indices (Just sel) = Map.lookup sel indices

  UI.element list UI.# UI.sink UI.selection bindex

  let bindices2 :: UI.Behavior (Map.Map Int a)
      bindices2 = Map.fromList . zip [0 ..] <$> bitems

      _selectionLB =
        UI.tidings undefined undefined -- bsel $ undefined
        -- lookupIndex <$> bindices2 UI.<@> UI.selectionChange list
      _elementLB = list

  return ZipperBox {..}

items = UI.mkWriteAttr $ \i x -> void $ do
  return x UI.# UI.set UI.children [] UI.#+ map (\i -> UI.option UI.#+ [i]) i

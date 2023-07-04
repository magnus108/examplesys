{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Item.List
  ( setup,
    tItemSelect,
    tItemFilter,
    eItemDelete,
  )
where

import qualified Data.Generic.HKD as HKD
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Item as Item
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Elements.Elements as Elements
import qualified Piece.Gui.Item.Behavior as Behavior
import qualified Reactive.Threepenny as R

data List = List
  { view :: UI.Element,
    tItemSelect :: R.Tidings (Maybe Db.DatabaseKey),
    tItemFilter :: R.Tidings String,
    eItemDelete :: R.Event Item.Item
  }

instance UI.Widget List where
  getElement = view

setup :: Monad.AppEnv -> UI.UI List
setup env = mdo
  itemEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.ItemEnv
  bDisplayItem <- liftIO $ Monad.runApp env Behavior.displayItem
  bItems <- liftIO $ Monad.runApp env Behavior.items

  ( (filterItem, filterItemView),
    (listBoxItem, listBoxItemView)
    ) <-
    Elements.mkSearchEntry bItems (Env.bSelectItem itemEnv) bDisplayItem (Env.bFilterItem itemEnv)

  (deleteBtn, deleteBtnView) <- Elements.mkButton "Slet"

  let bForm = HKD.construct <$> Env.bItemDeleteForm itemEnv

  _ <- UI.element deleteBtn UI.# UI.sink UI.enabled (isJust <$> bForm)

  view <-
    Elements.mkContainer
      [ Elements.mkBox
          UI.#+ [ UI.element filterItemView,
                  UI.element listBoxItemView,
                  UI.element deleteBtnView
                ]
      ]

  let tItemSelect = UI.userSelection listBoxItem
      tItemFilter = UI.userText filterItem
      eDelete = UI.click deleteBtn

  let eItemDelete = R.filterJust $ bForm UI.<@ eDelete

  return List {..}

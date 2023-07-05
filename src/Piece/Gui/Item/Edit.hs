{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Item.Edit
  ( setup,
    tItemSelect,
    tItemFilter,
    tItemEditForm,
    eItemEdit,
  )
where

import qualified Control.Lens as Lens
import qualified Data.Barbie as Barbie
import qualified Data.Generic.HKD as HKD
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Form.FormDataExpr as Form
import qualified Piece.Core.Item as Item
import qualified Piece.Core.ItemEditForm as ItemEditForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Item as Item
import qualified Piece.Gui.Elements.Elements as Elements
import qualified Piece.Gui.Item.Behavior as Behavior
import qualified Reactive.Threepenny as R

data Edit = Edit
  { view :: UI.Element,
    tItemSelect :: R.Tidings (Maybe Db.DatabaseKey),
    tItemFilter :: R.Tidings String,
    tItemEditForm :: R.Tidings ItemEditForm.Item,
    eItemEdit :: R.Event Item.Item
  }

instance UI.Widget Edit where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Edit
setup env = mdo
  itemEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.ItemEnv

  bDisplayItem <- liftIO $ Monad.runApp env Behavior.displayItem
  bItems <- liftIO $ Monad.runApp env Behavior.itemsEdit

  ( (filterItem, filterItemView),
    (listBoxItem, listBoxItemView)
    ) <-
    Elements.mkSearchEntry bItems (Env.bItemEditSelect itemEnv) bDisplayItem (Env.bItemEditFilter itemEnv)

  let bForm = Env.bItemEditForm itemEnv
      bFormData = HKD.bmap (\x -> Compose (fmap Form.getFormData (getCompose x))) <$> bForm

  -- Higgely
  bLookup <- liftIO $ Monad.runApp env Item.lookup
  let bCurrentSelection = (\x -> maybe mempty HKD.deconstruct x) <$> ((=<<) <$> bLookup <*> (Env.bItemEditSelect itemEnv))
      bFormConstruction = (\form currentSelection -> HKD.construct $ (Barbie.bzipWith (\x y -> liftA2 (\x1 y1 -> Form.constructData x1) (getCompose x) y) form currentSelection)) <$> bForm <*> bCurrentSelection

  -- this fine
  let bmItem = (\x -> getCompose (Lens.view (HKD.field @"name") x)) <$> bFormData
  (itemName, itemNameView) <- Elements.mkInput "Item" ((\x -> maybe "" Form.getContainer x) <$> bmItem)
  _ <- UI.element itemName UI.# UI.sink UI.enabled (isJust <$> bmItem)

  -- this fine
  (editBtn, editBtnView) <- Elements.mkButton "Edit"
  _ <- UI.element editBtn UI.# UI.sink UI.enabled (isJust <$> bFormConstruction)

  view <-
    Elements.mkContainer
      [ Elements.mkBox
          UI.#+ [ UI.element filterItemView,
                  UI.element listBoxItemView,
                  UI.element itemNameView,
                  UI.element editBtnView
                ]
      ]

  let tItemSelect = UI.userSelection listBoxItem
      tItemFilter = UI.userText filterItem
      tUserName = UI.userText itemName
      tItemEditForm = (\name -> HKD.build @Item.Item (Compose (Just (Form.StringExpr name)))) <$> tUserName
      eEdit = UI.click editBtn
      eItemEdit = R.filterJust $ bFormConstruction UI.<@ eEdit
  return Edit {..}

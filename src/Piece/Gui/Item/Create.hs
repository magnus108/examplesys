{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Item.Create
  ( setup,
    tItemCreateForm,
    eItemCreate,
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
import qualified Piece.Core.ItemCreateForm as ItemCreateForm
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Elements.Elements as Elements
import qualified Piece.Gui.Item.Behavior as Behavior
import qualified Reactive.Threepenny as R

data Create = Create
  { view :: UI.Element,
    tItemCreateForm :: R.Tidings ItemCreateForm.Item,
    eItemCreate :: R.Event Item.Item
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  itemEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.ItemEnv

  let bFormData = Env.bItemCreateForm itemEnv
      bForm = HKD.construct . HKD.bmap (\x -> fmap Form.constructData (getCompose x)) <$> bFormData

  (itemName, itemNameView) <- Elements.mkInput "Item" ((\x -> maybe "" Form.getFormData (getCompose (Lens.view (HKD.field @"name") x))) <$> bFormData)
  (createBtn, createBtnView) <- Elements.mkButton "Opret"

  _ <- UI.element createBtn UI.# UI.sink UI.enabled (isJust <$> bForm)

  view <-
    Elements.mkContainer
      [ Elements.mkBox
          UI.#+ [ UI.element itemNameView,
                  UI.element createBtnView
                ]
      ]

  let tUserName = UI.userText itemName
      tItemCreateForm = (\name -> HKD.build @Item.Item (Compose (Just (Form.StringExpr name)))) <$> tUserName
      eCreate = UI.click createBtn
      eItemCreate = R.filterJust $ bForm UI.<@ eCreate
  return Create {..}

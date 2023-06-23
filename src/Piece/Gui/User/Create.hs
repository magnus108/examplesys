{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Piece.Gui.User.Create
  ( setup,
    tUserCreate,
    tUserCreatingForm,
    tUserCreateForm,
    Create,
  )
where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Generic.HKD
import Data.Generic.HKD (field)
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Monad as Monad
import Piece.App.UserEnv (UserEnv (bUserCreateForm))
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Db.User as User
import qualified Piece.Gui.Checkbox.Checkbox as Checkbox
import Piece.Gui.User.List (mkButton, mkCheckbox, mkCheckboxer, mkContainer, mkInput, mkInputter)
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

data Create = Create
  { view :: UI.Element,
    tUserCreate :: R.Tidings (Maybe User.User),
    tUserCreatingForm :: R.Tidings UserCreateForm.User,
    tUserCreateForm :: R.Tidings UserCreateForm.User
  }

instance UI.Widget Create where
  getElement = view

-- modal

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  (userName, userNameView) <- mkInputter "Username" ((\y -> fromMaybe (Right "") (UserCreateForm.toName <$> y)) <$> UserEnv.bUserCreateForm userEnv)
  (userPassword, userPasswordView) <- mkInputter "Password" ((\y -> fromMaybe (Right "") (UserCreateForm.toPassword <$> y)) <$> UserEnv.bUserCreateForm userEnv)
  (userAdmin, userAdminView) <- mkCheckboxer "Admin" ((\y -> fromMaybe (Right False) (UserCreateForm.toRoles <$> y)) <$> UserEnv.bUserCreateForm userEnv) -- (maybe False (\x -> getCompose $ UserCreateForm.getFormData (x ^. field @"roles")) <$> UserEnv.bUserCreateForm userEnv)
  (createBtn, createBtnView) <- mkButton "Opret"

  view <-
    mkContainer
      [ UI.div
          UI.#. "box"
          UI.# UI.set
            UI.children
            [ UI.getElement userNameView,
              UI.getElement userPasswordView,
              UI.getElement userAdminView,
              createBtnView
            ]
      ]

  let tUserName = UI.userText userName
  let tUserPassword = UI.userText userPassword
  let tUserAdmin = Checkbox.userCheck userAdmin

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  let tUserCreateForm = UserCreateForm.form <$> tUserName <*> tUserPassword <*> tUserAdmin
      bUserCreateForm = UI.facts tUserCreateForm

      eCreate = UI.click createBtn

      tUserCreatingForm = R.tidings bUserCreateForm $ (bmap (const (Compose (Left ()))) <$> bUserCreateForm) UI.<@ eCreate

  (eUser, hUser) <- liftIO $ R.newEvent

  _ <- UI.onEvent eCreate $ \_ -> UI.liftIOLater $ Monad.runApp env $ UnliftIO.withRunInIO $ \run -> do
    userCreateForm <- R.currentValue bUserCreateForm
    val <- run $ User.create userCreateForm
    hUser val

  let tUserCreate = UI.tidings (UserEnv.bUserCreate userEnv) eUser
  return Create {..}

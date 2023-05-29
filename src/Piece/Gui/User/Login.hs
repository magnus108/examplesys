{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.User.Login
  ( setup,
    tUserLogin,
    tUserLoginForm,
    Create,
  )
where

import Data.Text
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.User.Behavior as Behavior
import Piece.Gui.User.List (mkButton, mkCheckbox, mkContainer, mkInput)
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

data Create = Create
  { view :: UI.Element,
    tUserLogin :: R.Tidings (Maybe Db.DatabaseKey),
    tUserLoginForm :: R.Tidings UserLoginForm.User
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  (userName, userNameView) <- mkInput "Username" (maybe "" UserLoginForm.name <$> UserEnv.bUserLoginForm userEnv)
  (userPassword, userPasswordView) <- mkInput "Password" (maybe "" (unpack . Password.unPasswordPlainText . UserLoginForm.password) <$> UserEnv.bUserLoginForm userEnv)
  (loginBtn, loginBtnView) <- mkButton "Login"

  -- GUI layout
  _ <- UI.element userName UI.# UI.sink UI.enabled bEnabled
  _ <- UI.element userPassword UI.# UI.sink UI.enabled bEnabled
  _ <- UI.element loginBtn UI.# UI.sink UI.enabled bEnabled

  view <-
    mkContainer
      [ UI.div
          UI.#. "box"
          UI.# UI.set
            UI.children
            [ UI.getElement userNameView,
              UI.getElement userPasswordView,
              loginBtnView
            ]
      ]

  let tUserName = UI.userText userName
  let tUserPassword = Password.PasswordPlainText . pack <$> UI.userText userPassword

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  let tUserLoginForm = UserLoginForm.user <$> tUserName <*> tUserPassword
      bUserLoginForm = UI.facts tUserLoginForm
      eLogin = UI.click loginBtn

  (eClick1, hClick1) <- liftIO R.newEvent
  (eClick2, hClick2) <- liftIO R.newEvent

  _ <- UI.onEvent eLogin $ \_ -> UI.liftIOLater $ Monad.runApp env $ UnliftIO.withRunInIO $ \run -> do
    hClick1 ()
    userCreateForm <- R.currentValue bUserLoginForm
    findUser <- R.currentValue bFindUser
    hClick2 (findUser userCreateForm)

  bEnabled <- R.stepper True $ Unsafe.head <$> R.unions [False <$ eClick1, True <$ eClick2]

  bFindUser <- liftIO $ Monad.runApp env $ Behavior.bFindUser

  let tUserLogin = UI.tidings (UserEnv.bUserLogin userEnv) $ eClick2

  return Create {..}

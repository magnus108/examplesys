{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.User.Login
  ( setup,
    tUserLogin,
    tUserLoginForm,
    Create,
  )
where

import Data.Text
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Checkbox.Checkbox as Checkbox
import qualified Piece.Gui.User.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified UnliftIO

data Create = Create
  { view :: UI.Element,
    tUserLogin :: R.Tidings (Maybe Db.DatabaseKey),
    tUserLoginForm :: R.Tidings UserLoginForm.User
  }

instance UI.Widget Create where
  getElement = view

-- modal

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  userName <- UI.entry (maybe "" UserLoginForm.name <$> UserEnv.bUserLoginForm userEnv)
  userPassword <- UI.entry (maybe "" (unpack . Password.unPasswordPlainText . UserLoginForm.password) <$> UserEnv.bUserLoginForm userEnv)
  loginBtn <- UI.button UI.#+ [UI.string "login"]

  view <-
    UI.div
      UI.# UI.set
        UI.children
        [ UI.getElement userName,
          UI.getElement userPassword,
          loginBtn
        ]

  let tUserName = UI.userText userName
  let tUserPassword = Password.PasswordPlainText . pack <$> UI.userText userPassword

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  let tUserLoginForm = UserLoginForm.user <$> tUserName <*> tUserPassword
      bUserLoginForm = UI.facts tUserLoginForm
      eLogin = UI.click loginBtn

  bFindUser <- liftIO $ Monad.runApp env $ Behavior.bFindUser
  let tUserLogin = UI.tidings (UserEnv.bUserLogin userEnv) $ bFindUser <*> bUserLoginForm UI.<@ eLogin

  return Create {..}

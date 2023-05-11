{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.User.Create
  ( setup,
    tUserCreate,
    tUserCreateForm,
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
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.User as User
import qualified Piece.Gui.Checkbox.Checkbox as Checkbox
import qualified Piece.Gui.User.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified UnliftIO

data Create = Create
  { view :: UI.Element,
    tUserCreate :: R.Tidings (Maybe User.User),
    tUserCreateForm :: R.Tidings UserCreateForm.User
  }

instance UI.Widget Create where
  getElement = view

-- modal

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  userName <- UI.entry (maybe "" UserCreateForm.name <$> UserEnv.bUserCreateForm userEnv)
  userPassword <- UI.entry (maybe "" (unpack . Password.unPasswordPlainText . UserCreateForm.password) <$> UserEnv.bUserCreateForm userEnv)
  userAdmin <- Checkbox.entry (maybe False UserCreateForm.admin <$> UserEnv.bUserCreateForm userEnv)
  createBtn <- UI.button UI.#+ [UI.string "Opret"]

  view <-
    UI.div
      UI.# UI.set
        UI.children
        [ UI.getElement userName,
          UI.getElement userPassword,
          UI.getElement userAdmin,
          createBtn
        ]

  let tUserName = UI.userText userName
  let tUserPassword = Password.PasswordPlainText . pack <$> UI.userText userPassword
  let tUserAdmin = Checkbox.userCheck userAdmin

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  let tUserCreateForm = UserCreateForm.user <$> tUserName <*> tUserPassword <*> tUserAdmin
      bUserCreateForm = UI.facts tUserCreateForm
      eCreate = UI.click createBtn

  eCreate' <- liftIO $ Monad.runApp env $ UnliftIO.withRunInIO $ \run -> do
    let e = R.unsafeMapIO (run . User.create) (bUserCreateForm UI.<@ eCreate)
    return e

  let tUserCreate = UI.tidings (UserEnv.bUserCreate userEnv) $ eCreate'

  return Create {..}

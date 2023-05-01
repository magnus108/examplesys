{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.User.Create
  ( setup,
    tUserCreate,
    tUserFormCreate,
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
import qualified Piece.Core.UserForm as UserForm
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.User.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified UnliftIO

data Create = Create
  { view :: UI.Element,
    tUserCreate :: R.Tidings (Maybe User.User),
    tUserFormCreate :: R.Tidings UserForm.User
  }

instance UI.Widget Create where
  getElement = view

-- checkbox
-- modal

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  user <- UI.entry (maybe "" UserForm.name <$> UserEnv.bUserFormCreate userEnv)
  userPassword <- UI.entry (maybe "" (unpack . Password.unPasswordPlainText . UserForm.password) <$> UserEnv.bUserFormCreate userEnv)
  createBtn <- UI.button UI.#+ [UI.string "Opret"]

  view <- UI.div UI.# UI.set UI.children [UI.getElement user, UI.getElement userPassword, createBtn]

  let tUser = UI.userText user
  let tUserPassword = Password.PasswordPlainText . pack <$> UI.userText userPassword

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  let tUserFormCreate = UserForm.user <$> tUser <*> tUserPassword <*> (pure [])
      bUserFormCreate = UI.facts tUserFormCreate
      eCreate = UI.click createBtn

  eCreate' <- liftIO $ Monad.runApp env $ UnliftIO.withRunInIO $ \run -> do
    let e = R.unsafeMapIO (run . createUser) (bUserFormCreate UI.<@ eCreate)
    return e

  let tUserCreate = UI.tidings (UserEnv.bUserCreate userEnv) $ eCreate'

  return Create {..}

createUser :: MonadIO m => UserForm.User -> m (Maybe User.User)
createUser form = do
  let formName = UserForm.name form
      formPassword = UserForm.password form
      formRoles = UserForm.roles form
  password <- Password.mkPasswordHash formPassword
  return $ case password of
    Nothing -> Nothing
    Just p -> Just (User.user formName p formRoles)

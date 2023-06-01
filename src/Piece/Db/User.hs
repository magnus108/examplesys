module Piece.Db.User
  ( lookup,
    getRoles,
    create,
    edit,
    bListBox,
    showUser,
    userEdit,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Core.UserEditForm as UserEditForm
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe User.User))
lookup = do
  userEnv <- Has.grab @UserEnv.UserEnv
  return $ flip Db.lookup <$> UserEnv.bDatabaseUser userEnv

showUser :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showUser = do
  bLookup <- lookup
  return $ (maybe "" User.name .) <$> bLookup

getRoles :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Int]))
getRoles = do
  bLookup <- lookup
  return $ (maybe [] User.roles .) <$> bLookup

create :: MonadIO m => UserCreateForm.User -> m (Maybe User.User)
create form = do
  let formName = UserCreateForm.name form
      formPassword = UserCreateForm.password form
      formAdmin = UserCreateForm.admin form
      roles = if formAdmin then [0, 1, 2] else [0, 1]
  password <- Password.mkPasswordHash formPassword
  return $ case password of
    Nothing -> Nothing
    Just p -> Just (User.user formName p roles)

edit :: MonadIO m => UserEditForm.User -> m (Maybe User.User)
edit form = do
  let formName = UserEditForm.name form
      formPassword = UserEditForm.password form
      formAdmin = UserEditForm.admin form
      roles = if formAdmin then [0, 1, 2] else [0, 1]
  password <- Password.mkPasswordHash formPassword
  return $ case password of
    Nothing -> Nothing
    Just p -> Just (User.user formName p roles)

userEdit :: User.User -> UserEditForm.User
userEdit user =
  let name = User.name user
      password = Password.PasswordPlainText ""
      roles = User.roles user
      admin = 2 `elem` roles
   in UserEditForm.user name password admin

bListBox :: (Env.WithUserEnv env m) => m (R.Behavior [Db.DatabaseKey])
bListBox = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  return $ Db.keys <$> bDatabaseUser

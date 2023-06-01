module Piece.App.UserEnv (UserEnv (..)) where

import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Core.UserEditForm as UserEditForm
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

data UserEnv = UserEnv
  { bDatabaseUser :: R.Behavior (Db.Database User.User),
    --- Create
    bUserCreateForm :: R.Behavior (Maybe UserCreateForm.User),
    bUserCreate :: R.Behavior (Maybe User.User),
    --- Login
    bUserLoginForm :: R.Behavior (Maybe UserLoginForm.User),
    bUserLogin :: R.Behavior (Maybe Db.DatabaseKey),
    --- Delete
    bSelectionUser :: R.Behavior (Maybe Db.DatabaseKey),
    bFilterUser :: R.Behavior String,
    bUserDelete :: R.Behavior (Maybe Db.DatabaseKey),
    --- Edit
    bSelectionUserEdit :: R.Behavior (Maybe Db.DatabaseKey),
    bFilterUserEdit :: R.Behavior String,
    bUserEditKeyValue :: R.Behavior (Maybe (Db.DatabaseKey, User.User)),
    bUserEditForm :: R.Behavior (Maybe UserEditForm.User)
  }

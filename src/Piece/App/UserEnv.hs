module Piece.App.UserEnv (UserEnv (..)) where

import qualified Piece.Core.User as User
import qualified Piece.Core.UserForm as UserForm
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

data UserEnv = UserEnv
  { bDatabaseUser :: R.Behavior (Db.Database User.User),
    bUserFormCreate :: R.Behavior (Maybe UserForm.User),
    bUserCreate :: R.Behavior (Maybe User.User)
  }

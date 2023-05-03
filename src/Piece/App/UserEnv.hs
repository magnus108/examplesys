module Piece.App.UserEnv (UserEnv (..)) where

import qualified Data.Time.Clock as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

data UserEnv = UserEnv
  { bDatabaseUser :: R.Behavior (Db.Database User.User),
    bUserCreateForm :: R.Behavior (Maybe UserCreateForm.User),
    bUserCreate :: R.Behavior (Maybe User.User),
    bUserLoginForm :: R.Behavior (Maybe UserLoginForm.User),
    bUserLogin :: R.Behavior (Maybe Db.DatabaseKey),
    bDatabaseToken :: R.Behavior (Db.Database Token.Token),
    bSelectionToken :: R.Behavior (Maybe Db.DatabaseKey),
    bTTL :: R.Behavior (Maybe Time.NominalDiffTime)
  }

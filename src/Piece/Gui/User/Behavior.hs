module Piece.Gui.User.Behavior
  ( displayUser,
    bFindUser,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Db.User as User
import qualified Reactive.Threepenny as R

bFindUser :: (Env.WithUserEnv env m) => m (R.Behavior (UserLoginForm.User -> Maybe Db.DatabaseKey))
bFindUser = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  bLookup <- User.lookup
  return $
    (\lookup db form -> find (maybe False (validateLogin form) . lookup) (Db.keys db))
      <$> bLookup
      <*> bDatabaseUser

validateLogin :: UserLoginForm.User -> User.User -> Bool
validateLogin form user =
  (User.name user == UserLoginForm.name form) && Password.verifyPassword (UserLoginForm.password form) (User.password user)

displayUser :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayUser = do
  bShow <- User.showUser
  return $ (UI.string .) <$> bShow

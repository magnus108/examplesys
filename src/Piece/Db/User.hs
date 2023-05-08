module Piece.Db.User
  ( lookup,
    getRoles,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Role as Role
import qualified Piece.Core.User as User
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe User.User))
lookup = do
  userEnv <- Has.grab @UserEnv.UserEnv
  return $ flip Db.lookup <$> UserEnv.bDatabaseUser userEnv

getRoles :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Int]))
getRoles = do
  bLookup <- lookup
  return $ (maybe [] User.roles .) <$> bLookup

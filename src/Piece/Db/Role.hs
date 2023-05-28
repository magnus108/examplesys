module Piece.Db.Role
  ( lookup,
    getPrivilege,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Role as Role
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Role.Role))
lookup = do
  roleEnv <- Has.grab @Env.RoleEnv
  return $ flip Db.lookup <$> Env.bDatabaseRole roleEnv

getPrivilege :: (Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Int]))
getPrivilege = do
  bLookup <- lookup
  return $ (maybe [] Role.privilege .) <$> bLookup

module Piece.Db.Privilege
  ( lookup,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithPrivilegeEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Privilege.Privilege))
lookup = do
  privilegeEnv <- Has.grab @Env.PrivilegeEnv
  return $ flip Db.lookup <$> Env.bDatabasePrivilege privilegeEnv

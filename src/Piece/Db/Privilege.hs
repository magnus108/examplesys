module Piece.Db.Privilege
  ( lookup,
  )
where

import qualified Data.Time.Clock as Time
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Db as Db
import qualified Piece.Db.User as User
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified UnliftIO as MonadUnliftIO

lookup :: (Env.WithPrivilegeEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Privilege.Privilege))
lookup = do
  privilegeEnv <- Has.grab @Env.PrivilegeEnv
  return $ flip Db.lookup <$> Env.bDatabasePrivilege privilegeEnv

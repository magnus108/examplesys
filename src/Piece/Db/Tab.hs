module Piece.Db.Tab
  ( lookup,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Tab.Tab))
lookup = do
  tabEnv <- Has.grab @Env.TabEnv
  return $ flip Db.lookup <$> Env.bDatabaseTab tabEnv

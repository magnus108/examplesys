module Piece.Db.Token
  ( lookup,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Token as Token
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Token.Token))
lookup = do
  tokenEnv <- Has.grab @Env.TokenEnv
  return $ flip Db.lookup <$> Env.bDatabaseToken tokenEnv

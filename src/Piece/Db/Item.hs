module Piece.Db.Item
  ( lookup,
    showItem,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Item as Item
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

lookup :: (Env.WithItemEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Item.Item))
lookup = do
  itemEnv <- Has.grab @Env.ItemEnv
  return $ flip Db.lookup <$> Env.bDatabaseItem itemEnv

showItem :: (Env.WithItemEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showItem = do
  bLookup <- lookup
  return $ (maybe "" Item.name .) <$> bLookup

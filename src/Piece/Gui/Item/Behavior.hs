module Piece.Gui.Item.Behavior
  ( displayItem,
    items,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Item as Item
import qualified Reactive.Threepenny as R

displayItem :: (Env.WithItemEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayItem = do
  bShow <- Item.showItem
  return $ (UI.string .) <$> bShow

items :: (Env.WithItemEnv env m) => m (R.Behavior [Db.DatabaseKey])
items = do
  itemEnv <- Has.grab @Env.ItemEnv
  let bDatabaseItem = Env.bDatabaseItem itemEnv
  let bFilterItem = isPrefixOf <$> Env.bFilterItem itemEnv
  bShowItem <- Item.showItem
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterItem
      <*> bShowItem
      <*> bDatabaseItem

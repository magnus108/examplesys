module Piece.Gui.Tab.Behavior
  ( showTab,
    displayButtonTab,
    displayViewTab,
    bZipperBox,
  )
where

import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Tab as DbTab
import qualified Reactive.Threepenny as R

showTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showTab = do
  bLookup <- DbTab.lookup
  return $ (maybe "" Tab.name .) <$> bLookup

displayButtonTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayButtonTab = do
  show <- showTab
  return $ (UI.string .) <$> show

displayViewTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayViewTab = do
  tabEnv <- Has.grab @Env.TabEnv
  let bView = flip Map.lookup <$> Env.bViewMapTab tabEnv
  return $ (fromMaybe (UI.string "not found") .) <$> bView

bZipperBox :: (Env.WithTabEnv env m) => UI.Behavior (String -> Bool) -> m (R.Behavior [Db.DatabaseKey])
bZipperBox bFilterTab = do
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  bShowTab <- showTab
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterTab
      <*> bShowTab
      <*> bDatabaseTab

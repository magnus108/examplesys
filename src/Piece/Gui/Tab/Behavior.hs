module Piece.Gui.Tab.Behavior
  ( showTab,
    displayTab,
    displayViewTab,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Tab as DbTab
import qualified Piece.Db.Tab as Tab
import qualified Reactive.Threepenny as R

showTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showTab = do
  bLookup <- DbTab.lookup
  return $ (maybe "" Tab.name .) <$> bLookup

displayTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayTab = do
  bShow <- showTab
  return $ (UI.string .) <$> bShow

displayViewTab :: (Env.WithTokenEnv env m, Env.WithPrivilegeEnv env m, Env.WithRoleEnv env m, Env.WithUserEnv env m, Env.WithTabEnv env m) => [UI.Element] -> m (R.Behavior (Db.DatabaseKey -> Maybe (UI.Element)))
displayViewTab tabsViews = do
  tabEnv <- Has.grab @Env.TabEnv
  tabs <- Tab.listBox
  let bMap = (\k -> Map.restrictKeys (Map.fromList (zip [0 ..] tabsViews)) (Set.fromList k)) <$> tabs
  let bView = flip Map.lookup <$> bMap
  return $ bView

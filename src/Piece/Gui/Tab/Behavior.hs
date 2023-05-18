module Piece.Gui.Tab.Behavior
  ( showTab,
    displayTab,
    listBox,
    displayViewTab,
  )
where

import Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Privilege as Privilege
import qualified Piece.Db.Tab as DbTab
import qualified Piece.Db.Token as Token
import qualified Reactive.Threepenny as R

showTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showTab = do
  bLookup <- DbTab.lookup
  return $ (maybe "" Tab.name .) <$> bLookup

privilegeTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Int]))
privilegeTab = do
  bLookup <- DbTab.lookup
  return $ (maybe [] Tab.privilege .) <$> bLookup

displayTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayTab = do
  bShow <- showTab
  return $ (UI.string .) <$> bShow

listBox :: (Env.WithUserEnv env m, Env.WithTokenEnv env m, Env.WithRoleEnv env m, Env.WithPrivilegeEnv env m, Env.WithTabEnv env m) => m (R.Behavior [Db.DatabaseKey])
listBox = do
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv

  bPrivilegeTab <- privilegeTab
  currentPrivilege <- Token.getPrivilege
  userEnv <- Has.grab @UserEnv.UserEnv
  tokenEnv <- Has.grab @Env.TokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv
  let gg = (=<<) <$> currentPrivilege <*> bSelectionToken
  let gg2 = fromMaybe [0] <$> gg
  let gg3 = (\xs ys -> not (disjoint xs ys)) <$> gg2
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> gg3
      <*> bPrivilegeTab
      <*> bDatabaseTab

displayViewTab :: (Env.WithTokenEnv env m, Env.WithPrivilegeEnv env m, Env.WithRoleEnv env m, Env.WithUserEnv env m, Env.WithTabEnv env m) => [UI.Element] -> m (R.Behavior (Db.DatabaseKey -> Maybe (UI.Element)))
displayViewTab tabsViews = do
  tabEnv <- Has.grab @Env.TabEnv
  tabs <- listBox
  let bMap = (\k -> Map.restrictKeys (Map.fromList (zip [0 ..] tabsViews)) (Set.fromList k)) <$> tabs
  let bView = flip Map.lookup <$> bMap
  return $ bView

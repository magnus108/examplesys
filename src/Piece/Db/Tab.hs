module Piece.Db.Tab
  ( lookup,
    getName,
    getPrivilegeIds,
    getPrivilege,
    listBox,
  )
where

import Data.List.Extra (disjoint)
import qualified Data.Map as Map
import Data.Profunctor
import qualified Data.Set as Set
import Data.Tuple.Extra
import Data.Tuple.Extra ((***))
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
import qualified Piece.Db.Token as Token
import qualified Reactive.Threepenny as R

lookup :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Tab.Tab))
lookup = do
  tabEnv <- Has.grab @Env.TabEnv
  return $ flip Db.lookup <$> Env.bDatabaseTab tabEnv

getName :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe String))
getName = do
  bLookup <- lookup
  return $ (fmap Tab.name .) <$> bLookup

getPrivilegeIds :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Db.DatabaseKey]))
getPrivilegeIds = do
  bLookup <- lookup
  return $ (fmap Tab.privilege .) <$> bLookup

getPrivilege :: (Env.WithTabEnv env m, Env.WithPrivilegeEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Privilege.Privilege]))
getPrivilege = do
  bGetPrivilege <- getPrivilegeIds
  bLookupPrivilege <- Privilege.lookup
  return $ (<=<) . mapM <$> bLookupPrivilege <*> bGetPrivilege

contains :: (Env.WithUserEnv env m, Env.WithPrivilegeEnv env m, Env.WithRoleEnv env m, Env.WithTabEnv env m, Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Db.DatabaseKey -> Bool))
contains = do
  bGetPrivilegeTab <- getPrivilege
  bGetPrivilegeToken <- Token.getPrivilege
  return $ (\f g -> curry $ lmap (firstM f <=< secondM g) $ maybe False (uncurry Privilege.contains)) <$> bGetPrivilegeToken <*> bGetPrivilegeTab

listBox :: (Env.WithPrivilegeEnv env m, Env.WithUserEnv env m, Env.WithTokenEnv env m, Env.WithRoleEnv env m, Env.WithTabEnv env m) => m (R.Behavior [Db.DatabaseKey])
listBox = do
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  tokenEnv <- Has.grab @Env.TokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv
  bContains <- contains
  return $
    (\p t db -> filter (maybe (const True) p t) (Db.keys db))
      <$> bContains
      <*> bSelectionToken
      <*> bDatabaseTab

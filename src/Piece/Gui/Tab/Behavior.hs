module Piece.Gui.Tab.Behavior
  ( showTab,
    displayTab,
    listBox,
    displayViewTab,
  )
where

import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Tab as DbTab
import qualified Reactive.Threepenny as R

displayViewTab :: (Env.WithTabEnv env m) => m (R.Behavior (Maybe Db.DatabaseKey -> UI.UI UI.Element))
displayViewTab = do
  tabEnv <- Has.grab @Env.TabEnv
  let bView = flip Map.lookup <$> Env.bViewMapTab tabEnv
  return $ (\f x -> fromMaybe (UI.string "not found") (f =<< x)) <$> bView

showTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showTab = do
  bLookup <- DbTab.lookup
  return $ (maybe "" Tab.name .) <$> bLookup

displayTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayTab = do
  bShow <- showTab
  return $ (UI.string .) <$> bShow

listBox :: (Env.WithTabEnv env m) => m (R.Behavior [Db.DatabaseKey])
listBox = do
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  let bFilterTab = pure (const True)
  bShowTab <- showTab
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterTab
      <*> bShowTab
      <*> bDatabaseTab

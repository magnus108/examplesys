module Piece.Gui.Tab.Behavior
  ( showTab,
    displayTab,
    bZipperBox,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import Graphics.UI.Threepenny.Elements as UI
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Tab as Tab
import qualified Piece.Db.Db as Db
import qualified Piece.Db.ListZipper as LZ
import qualified Piece.Db.Tab as DbTab
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

showTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showTab = do
  bLookup <- DbTab.lookup
  return $ (maybe "" Tab.name .) <$> bLookup

displayTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayTab = do
  show <- showTab
  return $ (UI.string .) <$> show

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

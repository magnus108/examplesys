module Piece.Gui.Tab.Behavior
  ( showTab,
    displayTab,
    displayButtonTab,
    displayButtonTabHandler,
    displayViewTab,
    bListBox,
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

showTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showTab = do
  bLookup <- DbTab.lookup
  return $ (maybe "" Tab.name .) <$> bLookup

displayTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayTab = do
  bShow <- showTab
  return $ (UI.string .) <$> bShow

class_ :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
class_ = do
  tabEnv <- Has.grab @Env.TabEnv
  let bSelectionTab = Env.bSelectionTab tabEnv
  return $ (\f x -> if f == Just x then "navbar-item is-active" else "navbar-item") <$> bSelectionTab

displayButtonTab :: (Env.WithTabEnv env m) => m (R.Behavior (Db.DatabaseKey -> UI.UI UI.Element))
displayButtonTab = do
  bDisplay <- displayTab
  bClass <- class_
  return $ (\f g x -> UI.a UI.#. g x UI.#+ [f x]) <$> bDisplay <*> bClass

displayButtonTabHandler :: (Env.WithTabEnv env m) => m (R.Behavior ((Db.DatabaseKey -> UI.UI ()) -> Db.DatabaseKey -> UI.UI UI.Element))
displayButtonTabHandler = do
  bDisplayButton <- displayButtonTab
  return $
    ( \display y x -> do
        btn <- display x
        UI.on UI.click btn $ \_ -> y x
        return btn
    )
      <$> bDisplayButton

displayViewTab :: (Env.WithTabEnv env m) => m (R.Behavior (Maybe Db.DatabaseKey -> UI.UI UI.Element))
displayViewTab = do
  tabEnv <- Has.grab @Env.TabEnv
  let bView = flip Map.lookup <$> Env.bViewMapTab tabEnv
  return $ (\f x -> fromMaybe (UI.string "not found") (f =<< x)) <$> bView

bListBox :: (Env.WithTabEnv env m) => m (R.Behavior [Db.DatabaseKey])
bListBox = do
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  let bFilterTab = pure (const True)
  bShowTab <- showTab
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterTab
      <*> bShowTab
      <*> bDatabaseTab

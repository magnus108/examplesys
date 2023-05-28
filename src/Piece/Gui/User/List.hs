{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.User.List
  ( setup,
    tUserFilter,
    tUserSelection,
    Create,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Gui.User.Behavior as Behavior
import qualified Reactive.Threepenny as R

data Create = Create
  { view :: UI.Element,
    tUserSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tUserFilter :: R.Tidings String
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  (searchEntry, filterUser, listBoxUser) <- mkSearchEntry bOtherUsers (UserEnv.bSelectionUser userEnv) bDisplayUser (UserEnv.bFilterUser userEnv)

  view <- mkContainer [UI.element searchEntry]

  let tUserSelection = UI.userSelection listBoxUser
  let tUserFilter = UI.userText filterUser

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  bDisplayUser <- liftIO $ Monad.runApp env $ Behavior.displayUser
  bOtherUsers <- liftIO $ Monad.runApp env $ Token.bOtherUsers

  return Create {..}

mkListBox :: forall a. (Ord a) => R.Behavior [a] -> R.Behavior (Maybe a) -> R.Behavior (a -> UI.UI UI.Element) -> UI.UI (UI.ListBox a, UI.Element)
mkListBox bItems bSel bDisplay = do
  listBox <- UI.listBox bItems bSel bDisplay
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.div
                UI.#. "control is-expanded"
                UI.#+ [ UI.div
                          UI.#. "select is-multiple is-fullwidth"
                          UI.#+ [UI.element listBox UI.# UI.set (UI.attr "size") "5" UI.# UI.set UI.style [("height", "auto")]]
                      ]
            ]

  return (listBox, view)

mkInput :: UI.Behavior String -> UI.UI (UI.TextEntry, UI.Element)
mkInput bFilterItem = do
  filterItem <- UI.entry bFilterItem
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string "Søg"],
              UI.div UI.#. "control" UI.#+ [UI.element filterItem UI.#. "input"]
            ]
  return (filterItem, view)

mkSearchEntry ::
  forall a.
  (Ord a) =>
  UI.Behavior [a] ->
  UI.Behavior (Maybe a) ->
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.Behavior String ->
  UI.UI (UI.Element, UI.TextEntry, UI.ListBox a)
mkSearchEntry bItems bSel bDisplay bFilterItem = do
  (filter, filterView) <- mkInput bFilterItem
  (listBox, listBoxView) <- mkListBox bItems bSel bDisplay
  --    counterView                 <- mkCounter bItems
  view <- UI.div UI.#. "box" UI.#+ [UI.element filterView, UI.element listBoxView] -- , UI.element counterView]
  return (view, filter, listBox)

mkContainer :: [UI.UI UI.Element] -> UI.UI UI.Element
mkContainer elems =
  UI.div UI.#. "section is-medium" UI.#+ [UI.div UI.#. "container" UI.#+ elems]

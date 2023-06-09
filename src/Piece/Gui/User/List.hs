{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Gui.User.List
  ( setup,
    tUserFilter,
    tUserSelection,
    tUserDelete,
    Create,
    mkSearch,
    mkInput,
    mkInputter,
    mkInputter2,
    mkContainer,
    mkCheckbox,
    mkCheckboxer,
    mkCheckboxer2,
    mkButton,
  )
where

import qualified Data.Functor.Product as Product
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as Form
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Gui.Checkbox.Checkbox as Checkbox
import qualified Piece.Gui.User.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data Create = Create
  { view :: UI.Element,
    tUserSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tUserFilter :: R.Tidings String,
    tUserDelete :: R.Tidings (Maybe Db.DatabaseKey)
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  ((filterUser, filterUserView), (listBoxUser, listBoxUserView)) <- mkSearchEntry bOtherUsers (UserEnv.bSelectionUser userEnv) bDisplayUser (UserEnv.bFilterUser userEnv)

  (deleteBtn, deleteBtnView) <- mkButton "Slet"

  _ <- UI.element deleteBtn UI.# UI.sink UI.enabled bAvailable

  view <-
    mkContainer
      [ mkBox
          UI.#+ [ UI.element filterUserView,
                  UI.element listBoxUserView,
                  UI.element deleteBtnView
                ]
      ]

  let tUserSelection = UI.userSelection listBoxUser
      bUserSelection = UI.facts tUserSelection

  let tUserFilter = UI.userText filterUser
      eDelete = UI.click deleteBtn

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv

  bDisplayUser <- liftIO $ Monad.runApp env Behavior.displayUser

  let bFilterUser = isPrefixOf <$> UserEnv.bFilterUser userEnv
  let bSelection = UserEnv.bSelectionUser userEnv
  bAvailable <- liftIO $ Monad.runApp env (Token.availableSelection bSelection bFilterUser)
  bOtherUsers <- liftIO $ Monad.runApp env (Token.bOtherUsersFilter bFilterUser)

  let tUserDelete = UI.tidings (UserEnv.bUserDelete userEnv) (bUserSelection UI.<@ eDelete)

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

mkSearch :: R.Behavior String -> UI.UI (UI.TextEntry, UI.Element)
mkSearch = mkInput "Søg"

mkInputter :: String -> UI.Behavior ((UserCreateForm.Config, String)) -> UI.UI (UI.TextEntry, UI.Element)
mkInputter label bFilterItem = do
  filterItem <- UI.entry ((\(a, b) -> b) <$> bFilterItem)
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string label],
              UI.div UI.#. "control" UI.#+ [UI.element filterItem UI.#. "input" UI.# UI.sink UI.enabled ((\(UserCreateForm.Config a, b) -> a) <$> bFilterItem)]
            ]
  return (filterItem, view)

-- mkInputter2 :: _ -- String -> R.Behavior (Product.Product (Const UserCreateForm.Config) UserCreateForm.FormDataExpr a) -> UI.UI (UI.TextEntry, UI.Element)
mkInputter2 label bFilterItem = do
  filterItem <- UI.entry ((\(Product.Pair a b) -> Form.getFormData b) <$> bFilterItem)
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string label],
              UI.div UI.#. "control" UI.#+ [UI.element filterItem UI.#. "input" UI.# UI.sink UI.enabled ((\(Product.Pair a b) -> UserCreateForm.enabled (getConst a)) <$> bFilterItem)]
            ]
  return (filterItem, view)

mkCheckboxer2 :: String -> R.Behavior (Product.Product (Const UserCreateForm.Config) UserCreateForm.FormDataExpr [Int]) -> UI.UI (Checkbox.CheckboxEntry, UI.Element)
mkCheckboxer2 label bCheck = do
  elem <- Checkbox.entry ((\(Product.Pair a b) -> Form.getFormData b) <$> bCheck)
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string label],
              UI.div UI.#. "control" UI.#+ [UI.element elem UI.#. "checkbox" UI.# UI.sink UI.enabled ((\(Product.Pair a b) -> UserCreateForm.enabled (getConst a)) <$> bCheck)]
            ]
  return (elem, view)

mkInput :: String -> UI.Behavior String -> UI.UI (UI.TextEntry, UI.Element)
mkInput label bFilterItem = do
  filterItem <- UI.entry bFilterItem
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string label],
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
  UI.UI ((UI.TextEntry, UI.Element), (UI.ListBox a, UI.Element))
mkSearchEntry bItems bSel bDisplay bFilterItem = do
  filter <- mkSearch bFilterItem
  listBox <- mkListBox bItems bSel bDisplay
  --    counterView                 <- mkCounter bItems
  return (filter, listBox)

mkBox :: UI.UI UI.Element
mkBox = UI.div UI.#. "box"

mkContainer :: [UI.UI UI.Element] -> UI.UI UI.Element
mkContainer elems =
  UI.div UI.#. "section is-medium" UI.#+ [UI.div UI.#. "container" UI.#+ elems]

mkButton :: String -> UI.UI (UI.Element, UI.Element)
mkButton title = do
  button <- UI.button UI.#+ [UI.string title]
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [UI.div UI.#. "control" UI.#+ [UI.element button UI.#. "button"]]
  return (button, view)

mkCheckboxer :: String -> R.Behavior (UserCreateForm.Config, Bool) -> UI.UI (Checkbox.CheckboxEntry, UI.Element)
mkCheckboxer label bCheck = do
  elem <- Checkbox.entry ((\(a, b) -> b) <$> bCheck)
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string label],
              UI.div UI.#. "control" UI.#+ [UI.element elem UI.#. "checkbox" UI.# UI.sink UI.enabled ((\(UserCreateForm.Config a, b) -> a) <$> bCheck)]
            ]
  return (elem, view)

mkCheckbox :: String -> R.Behavior Bool -> UI.UI (Checkbox.CheckboxEntry, UI.Element)
mkCheckbox label bCheck = do
  elem <- Checkbox.entry bCheck
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.label UI.#. "label" UI.#+ [UI.string label],
              UI.div UI.#. "control" UI.#+ [UI.element elem UI.#. "checkbox"]
            ]
  return (elem, view)

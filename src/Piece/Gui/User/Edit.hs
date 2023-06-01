{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.User.Edit
  ( setup,
    tUserEditForm,
    tUserFilter,
    tUserSelection,
    tUserEditKeyValue,
    Edit,
    mkSearch,
    mkInput,
    mkContainer,
    mkCheckbox,
    mkButton,
  )
where

import Data.Text (pack, unpack)
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserEditForm as UserEditForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Db.User as User
import qualified Piece.Gui.Checkbox.Checkbox as Checkbox
import qualified Piece.Gui.User.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

data Edit = Edit
  { view :: UI.Element,
    tUserSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tUserFilter :: R.Tidings String,
    tUserEditKeyValue :: R.Tidings (Maybe (Db.DatabaseKey, User.User)),
    tUserEditForm :: R.Tidings UserEditForm.User
  }

instance UI.Widget Edit where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Edit
setup env = mdo
  ((filterUser, filterUserView), (listBoxUser, listBoxUserView)) <- mkSearchEntry bOtherUsers (UserEnv.bSelectionUserEdit userEnv) bDisplayUser (UserEnv.bFilterUserEdit userEnv)

  (userName, userNameView) <- mkInput "Username" (maybe "" UserEditForm.name <$> UserEnv.bUserEditForm userEnv)
  (userPassword, userPasswordView) <- mkInput "Password" (maybe "" (unpack . Password.unPasswordPlainText . UserEditForm.password) <$> UserEnv.bUserEditForm userEnv)
  (userAdmin, userAdminView) <- mkCheckbox "Admin" (maybe False UserEditForm.admin <$> UserEnv.bUserEditForm userEnv)
  (editBtn, editBtnView) <- mkButton "Change"

  -- GUI layout
  _ <- UI.element userName UI.# UI.sink UI.enabled bEnabled
  _ <- UI.element userPassword UI.# UI.sink UI.enabled bEnabled
  _ <- UI.element userAdmin UI.# UI.sink UI.enabled bEnabled
  _ <- UI.element editBtn UI.# UI.sink UI.enabled ((&&) <$> bEnabled <*> (isJust <$> UserEnv.bUserEditForm userEnv))

  let tUserName = UI.userText userName
  let tUserPassword = Password.PasswordPlainText . pack <$> UI.userText userPassword
  let tUserAdmin = Checkbox.userCheck userAdmin

  view <-
    mkContainer
      [ mkBox
          UI.#+ [ UI.element filterUserView,
                  UI.element listBoxUserView,
                  UI.div
                    UI.#. "box"
                    UI.# UI.set
                      UI.children
                      [ UI.getElement userNameView,
                        UI.getElement userPasswordView,
                        UI.getElement userAdminView,
                        editBtnView
                      ]
                ]
      ]

  userEnv <- liftIO $ Monad.runApp env $ Has.grab @UserEnv.UserEnv
  let bSelection = UserEnv.bSelectionUserEdit userEnv
  let bFilterUser = isPrefixOf <$> UserEnv.bFilterUserEdit userEnv

  bDisplayUser <- liftIO $ Monad.runApp env Behavior.displayUser
  bAvailable <- liftIO $ Monad.runApp env (Token.availableSelection bSelection bFilterUser)
  bOtherUsers <- liftIO $ Monad.runApp env (Token.bOtherUsersFilter bFilterUser)

  let tUserSelection = UI.userSelection listBoxUser
      bUserSelection = UI.facts tUserSelection
      eUserSelection = UI.rumors tUserSelection

  let tUserFilter = UI.userText filterUser

  let tUserEditForm = UserEditForm.user <$> tUserName <*> tUserPassword <*> tUserAdmin
      bUserEditForm = UI.facts tUserEditForm
      eEdit = UI.click editBtn

  (eClick1, hClick1) <- liftIO R.newEvent
  (eClick2, hClick2) <- liftIO R.newEvent

  _ <- UI.onEvent eEdit $ \_ -> UI.liftIOLater $ Monad.runApp env $ UnliftIO.withRunInIO $ \run -> do
    hClick1 ()
    userEditForm <- R.currentValue bUserEditForm
    val <- run $ User.edit userEditForm
    hClick2 val

  bEnabledLoading <- R.stepper True $ Unsafe.head <$> R.unions [False <$ eClick1, True <$ eClick2]

  let bEnabled = (&&) <$> bEnabledLoading <*> (isJust <$> UserEnv.bSelectionUserEdit userEnv)

  let tUserEditKeyValue = UI.tidings (UserEnv.bUserEditKeyValue userEnv) (liftA2 (,) <$> bUserSelection UI.<@> eClick2)

  return Edit {..}

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

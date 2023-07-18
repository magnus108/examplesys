{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tUserFilter,
    tItemFilter,
    tLoanCreateForm,
    tLoanCreate,
    Create,
  )
where

import qualified Control.Lens as Lens
import qualified Data.Barbie as Barbie
import qualified Data.Generic.HKD as HKD
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Form.FormDataExpr as Form
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.LoanCreateForm as LoanCreateForm
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Elements.Elements as Elements
import qualified Piece.Gui.Item.Behavior as BehaviorItem
import qualified Piece.Gui.Loan.Behavior as Behavior
import qualified Piece.Gui.User.Behavior as BehaviorUser
import qualified Reactive.Threepenny as R

data Create = Create
  { view :: UI.Element,
    tUserFilter :: R.Tidings String,
    tItemFilter :: R.Tidings String,
    tLoanCreateForm :: R.Tidings LoanCreateForm.Loan,
    tLoanCreate :: R.Tidings (Maybe Loan.Loan)
  }

instance UI.Widget Create where
  getElement = view

mkSearchEntry ::
  Ord a =>
  UI.Behavior [a] ->
  UI.Behavior (Form.Form a) ->
  UI.Behavior (a -> UI.UI UI.Element) ->
  UI.Behavior String ->
  UI.UI ((UI.TextEntry, UI.Element), (ListBox a, UI.Element))
mkSearchEntry bItems bSel bDisplay bFilterItem = do
  filter <- Elements.mkSearch bFilterItem
  listBox <- mkListBox bItems bSel bDisplay
  -- counterView <- mkCounter bItems
  return (filter, listBox)

data ListBox a = ListBox
  { _elementLB :: UI.Element,
    _selectionLB :: R.Tidings (Form.Form a)
  }

instance UI.Widget (ListBox a) where
  getElement = _elementLB

userSelection :: ListBox a -> R.Tidings (Form.Form a)
userSelection = _selectionLB

mkListBox :: Ord a => R.Behavior [a] -> R.Behavior (Form.Form a) -> R.Behavior (a -> UI.UI UI.Element) -> UI.UI (ListBox a, UI.Element)
mkListBox bItems bSel bDisplay = do
  let bSel' = (\x -> Form.constructData . Form.to x . Form.constructData <$> Form.from x) <$> bSel
  listBox <- UI.listBox bItems bSel' bDisplay
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
  let tSelect = UI.userSelection listBox
      eSelect = R.rumors tSelect

  let tSelect' = R.tidings bSel $ (\orig new -> Form.Form (Form.to orig <$> new) (Form.to orig)) <$> bSel R.<@> eSelect
  let listBox' = ListBox (UI.getElement listBox) tSelect'
  return (listBox', view)

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  loanEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.LoanEnv

  let bForm = Env.bLoanCreateForm loanEnv
      bConstruction = HKD.construct . Barbie.bmap (fmap Form.constructData . Form.from) <$> bForm

  let userFilter = Env.bLoanCreateUserFilter loanEnv
      itemFilter = Env.bLoanCreateItemFilter loanEnv

  let itemSelect = Lens.view (HKD.field @"item") <$> bForm
      userSelect = Lens.view (HKD.field @"user") <$> bForm

  bDisplayUser <- liftIO $ Monad.runApp env BehaviorUser.displayUser
  bUsers <- liftIO $ Monad.runApp env Behavior.users

  bDisplayItem <- liftIO $ Monad.runApp env BehaviorItem.displayItem
  bItems <- liftIO $ Monad.runApp env Behavior.items

  ( (filterUser, filterUserView),
    (listBoxUser, listBoxUserView)
    ) <-
    mkSearchEntry bUsers userSelect bDisplayUser userFilter

  ( (filterItem, filterItemView),
    (listBoxItem, listBoxItemView)
    ) <-
    mkSearchEntry bItems itemSelect bDisplayItem itemFilter

  (createBtn, createBtnView) <- Elements.mkButton "Opret"

  _ <- UI.element createBtn UI.# UI.sink UI.enabled (isJust <$> bConstruction)

  view <-
    Elements.mkContainer
      [ Elements.mkBox
          UI.#+ [ UI.element filterUserView,
                  UI.element listBoxUserView,
                  UI.element filterItemView,
                  UI.element listBoxItemView,
                  UI.element createBtnView
                ]
      ]

  let tItemSelect = userSelection listBoxItem
      tItemFilter = UI.userText filterItem

  let tUserSelect = userSelection listBoxUser
      tUserFilter = UI.userText filterUser

  let eCreate = UI.click createBtn

  let tLoanCreateForm = HKD.build @Loan.Loan <$> tUserSelect <*> tItemSelect

  let tLoanCreate = R.tidings bConstruction $ bConstruction UI.<@ eCreate

  return Create {..}

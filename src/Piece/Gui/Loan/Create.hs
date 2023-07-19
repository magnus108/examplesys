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
import qualified Piece.Core.Form.FormDataExpr2 as Form
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
  UI.Behavior [Int] ->
  UI.Behavior (Form.Form Int) ->
  UI.Behavior (Int -> UI.UI UI.Element) ->
  UI.Behavior String ->
  UI.UI ((UI.TextEntry, UI.Element), ((R.Tidings (Form.Form Int), UI.Element), UI.Element))
mkSearchEntry bItems bSel bDisplay bFilterItem = do
  filter <- Elements.mkSearch bFilterItem
  listBox <- mkListBox bItems bSel bDisplay
  -- counterView <- mkCounter bItems
  return (filter, listBox)

loller :: R.Behavior (Form.Form Int) -> UI.UI (R.Tidings (Form.Form Int), UI.Element)
loller bSel = undefined

-- let bSel' = (\x -> Form.to x . Form.constructData <$> Form.from x) <$> bSel

mkListBox :: R.Behavior [Int] -> R.Behavior (Form.Form Int) -> R.Behavior (Int -> UI.UI UI.Element) -> UI.UI ((R.Tidings (Form.Form Int), UI.Element), UI.Element)
mkListBox bItems bSel bDisplay = do
  let bSel' = (\x -> Form.to x . Form.constructData <$> Form.from x) <$> bSel
  listBox <- UI.listBox bItems bSel' bDisplay
  (tSelect, elem) <- loller bSel
  view <-
    UI.div
      UI.#. "field"
      UI.#+ [ UI.div
                UI.#. "control is-expanded"
                UI.#+ [ UI.div
                          UI.#. "select is-multiple is-fullwidth"
                          UI.#+ [UI.element elem UI.# UI.set (UI.attr "size") "5" UI.# UI.set UI.style [("height", "auto")]]
                      ]
            ]
  --  let tSelect = UI.userSelection listBox
  -- let eSelect = R.rumors tSelect

  --  let tSelect' = R.tidings bSel $ (\orig new -> Form.Form (Form.SelectExpr <$> new) (Form.to orig)) <$> bSel R.<@> eSelect
  --  let listBox' = ListBox (UI.getElement listBox) undefined -- tSelect'
  return ((tSelect, elem), view)

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
    ((tUserSelect, listBoxUser), listBoxUserView)
    ) <-
    mkSearchEntry bUsers userSelect bDisplayUser userFilter

  ( (filterItem, filterItemView),
    ((tItemSelect, listBoxItem), listBoxItemView)
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

  let tItemFilter = UI.userText filterItem
      tUserFilter = UI.userText filterUser

  let eCreate = UI.click createBtn

  let tLoanCreateForm = HKD.build @Loan.Loan <$> tUserSelect <*> tItemSelect

  let tLoanCreate = R.tidings bConstruction $ bConstruction UI.<@ eCreate

  return Create {..}

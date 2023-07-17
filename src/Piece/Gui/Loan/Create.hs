{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tUserFilter,
    tUserSelect,
    tItemFilter,
    tItemSelect,
    tLoanCreateForm,
    Create,
  )
where

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
    tUserSelect :: R.Tidings (Maybe Db.DatabaseKey),
    tUserFilter :: R.Tidings String,
    tItemSelect :: R.Tidings (Maybe Db.DatabaseKey),
    tItemFilter :: R.Tidings String,
    tLoanCreateForm :: R.Tidings LoanCreateForm.Loan
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  loanEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.LoanEnv
  let userSelect = Env.bLoanCreateUserSelect loanEnv
      userFilter = Env.bLoanCreateUserFilter loanEnv

  let itemSelect = Env.bLoanCreateItemSelect loanEnv
      itemFilter = Env.bLoanCreateItemFilter loanEnv

  bDisplayUser <- liftIO $ Monad.runApp env BehaviorUser.displayUser
  bUsers <- liftIO $ Monad.runApp env Behavior.users

  bDisplayItem <- liftIO $ Monad.runApp env BehaviorItem.displayItem
  bItems <- liftIO $ Monad.runApp env Behavior.items

  ( (filterUser, filterUserView),
    (listBoxUser, listBoxUserView)
    ) <-
    Elements.mkSearchEntry bUsers userSelect bDisplayUser userFilter

  ( (filterItem, filterItemView),
    (listBoxItem, listBoxItemView)
    ) <-
    Elements.mkSearchEntry bItems itemSelect bDisplayItem itemFilter

  (createBtn, createBtnView) <- Elements.mkButton "Opret"

  let bForm = Env.bLoanCreateForm loanEnv
      bFormConstruction = HKD.construct . Barbie.bmap (fmap Form.constructData . Form.from) <$> bForm

  _ <- UI.element createBtn UI.# UI.sink UI.enabled (isJust <$> bFormConstruction)

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

  let tItemSelect = UI.userSelection listBoxItem
      tItemFilter = UI.userText filterItem

  let tUserSelect = UI.userSelection listBoxUser
      tUserFilter = UI.userText filterUser

  let eCreate = UI.click createBtn

  let tFormData = HKD.build @Loan.Loan <$> tUserSelect <*> tItemSelect :: R.Tidings (HKD.HKD Loan.Loan Maybe)
  --  let bFormData = (\form userId itemId -> HKD.build @Loan.Loan (Form.Form (Just (Form.to (Lens.view (HKD.field @"name") form) name))
  --                                                                       (Form.to (Lens.view (HKD.field @"name") form)))) <$> bForm <*>
  let tLoanCreateForm = R.tidings bForm $ undefined -- bFormData UI.<@ eCreate
  return Create {..}

{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tDatabaseLoan,
    tLoanFilter,
    eCreate,
    Create,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Loan.Behavior as Behavior
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data Create = Create
  { view :: UI.Element,
    tDatabaseLoan :: R.Tidings (Db.Database Loan.Loan),
    tLoanSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tLoanFilter :: R.Tidings String,
    eCreate :: R.Event ()
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  listBoxLoan <- Widgets.listBox bListBoxLoans (Env.bSelectionLoan loanEnv) bDisplayLoan
  filterLoan <- Widgets.entry (Env.bFilterLoan loanEnv)

  bob <- UI.string "bob2"
  btn <- Elements.button UI.# UI.set UI.children [bob]
  view <- Elements.div UI.# UI.set UI.children [UI.getElement listBoxLoan, UI.getElement filterLoan, btn]

  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = Widgets.userText filterLoan
      tFilterLoan = isPrefixOf <$> tLoanFilter
      bFilterLoan = R.facts tFilterLoan

  let eCreate = Events.click btn

  loanEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  bDisplayLoan <- liftIO $ Monad.runApp env $ Behavior.displayLoan

  bListBoxLoans <- liftIO $ Monad.runApp env $ Behavior.bListBox bFilterLoan

  let tDatabaseLoan =
        R.tidings bDatabaseLoan $
          Unsafe.head
            <$> R.unions
              [ Db.create (Loan.loan "dadda")
                  <$> bDatabaseLoan
                  R.<@ eCreate
              ]

  return Create {..}

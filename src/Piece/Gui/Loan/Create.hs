{-# LANGUAGE RecursiveDo #-}

module Piece.Gui.Loan.Create
  ( setup,
    tLoanFilter,
    Create,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Loan.Behavior as Behavior
import qualified Reactive.Threepenny as R

data Create = Create
  { view :: UI.Element,
    tLoanSelection :: R.Tidings (Maybe Db.DatabaseKey),
    tLoanFilter :: R.Tidings String
  }

instance UI.Widget Create where
  getElement = view

setup :: Monad.AppEnv -> UI.UI Create
setup env = mdo
  listBoxLoan <- Widgets.listBox bListBoxLoans (Env.bSelectionLoan loanEnv) bDisplayLoan
  filterLoan <- Widgets.entry (Env.bFilterLoan loanEnv)

  view <- Elements.div UI.# UI.set UI.children [UI.getElement listBoxLoan, UI.getElement filterLoan]

  let tLoanSelection = Widgets.userSelection listBoxLoan
  let tLoanFilter = Widgets.userText filterLoan

  loanEnv <- liftIO $ Monad.runApp env $ Has.grab @Env.LoanEnv

  bDisplayLoan <- liftIO $ Monad.runApp env $ Behavior.displayLoan
  bListBoxLoans <- liftIO $ Monad.runApp env $ Behavior.bListBox

  return Create {..}

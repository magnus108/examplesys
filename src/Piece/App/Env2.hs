{-# LANGUAGE TemplateHaskell #-}

module Piece.App.Env2
  ( AppBehavior (..),
    appBehavior,
    LoanBehavior (..),
    loanBehavior,
    bDatabaseLoan,
    bSelectionUser,
    bSelectionItem,
    bSelectionLoan,
    bFilterUser,
    bFilterItem,
    bFilterLoan,
    bModalState,
    HasLoanBehavior,
    HasAppBehavior,
  )
where

import qualified Control.Lens.TH as Lens
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R

data LoanBehavior = LoanBehavior
  { _bDatabaseLoan :: R.Behavior (Db.Database Loan.Loan),
    _bSelectionUser :: R.Behavior (Maybe Db.DatabaseKey),
    _bSelectionItem :: R.Behavior (Maybe Db.DatabaseKey),
    _bSelectionLoan :: R.Behavior (Maybe Db.DatabaseKey),
    _bFilterUser :: R.Behavior String,
    _bFilterItem :: R.Behavior String,
    _bFilterLoan :: R.Behavior String,
    _bModalState :: R.Behavior Bool
  }

Lens.makeClassy ''LoanBehavior

newtype AppBehavior = AppBehavior
  { _appLoanBehavior :: LoanBehavior
  }

Lens.makeClassy ''AppBehavior

instance HasLoanBehavior AppBehavior where
  loanBehavior = appBehavior . appLoanBehavior

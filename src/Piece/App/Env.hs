{-# LANGUAGE DataKinds #-}

module Piece.App.Env
  ( Env (..),
    LoanEnv (..),
    WithLoanEnv,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import Piece.CakeSlayer.Has (Field (..), Has)
import Piece.Core.Item (Item)
import Piece.Core.Loan (Loan)
import Piece.Core.User (User)
import Piece.Db.Db (Database, DatabaseKey)
import qualified Reactive.Threepenny as R

data Env (m :: Type -> Type) = Env
  { loanEnv :: LoanEnv,
    window :: UI.Window
  }
  deriving (Has LoanEnv) via Field "loanEnv" (Env m)
  deriving (Has UI.Window) via Field "window" (Env m)

type WithLoanEnv env m = (MonadReader env m, Has LoanEnv env)

data LoanEnv = LoanEnv
  { bDatabaseLoan :: R.Behavior (Database Loan),
    eDatabaseLoan :: R.Event [Database Loan],
    bSelectionUser :: R.Behavior (Maybe DatabaseKey),
    bSelectionItem :: R.Behavior (Maybe DatabaseKey),
    bSelectionLoan :: R.Behavior (Maybe DatabaseKey),
    bFilterUser :: R.Behavior String,
    bFilterItem :: R.Behavior String,
    bFilterLoan :: R.Behavior String,
    bModalState :: R.Behavior Bool
  }

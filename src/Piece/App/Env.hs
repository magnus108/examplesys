{-# LANGUAGE DataKinds #-}

module Piece.App.Env
  ( Env (..),
    LoanEnv (..),
    TabEnv (..),
    WithLoanEnv,
    WithTabEnv,
  )
where

import Piece.CakeSlayer.Has (Field (..), Has)
import Piece.Core.Loan (Loan)
import qualified Piece.Core.Tab as Tab
import Piece.Db.Db (Database, DatabaseKey)
import qualified Reactive.Threepenny as R

data Env (m :: Type -> Type) = Env
  { loanEnv :: LoanEnv,
    tabEnv :: TabEnv
  }
  deriving (Has LoanEnv) via Field "loanEnv" (Env m)
  deriving (Has TabEnv) via Field "tabEnv" (Env m)

type WithLoanEnv env m = (MonadReader env m, Has LoanEnv env)

type WithTabEnv env m = (MonadReader env m, Has TabEnv env)

data TabEnv = TabEnv
  { bDatabaseTab :: R.Behavior (Database Tab.Tab)
  }

data LoanEnv = LoanEnv
  { bDatabaseLoan :: R.Behavior (Database Loan),
    bSelectionUser :: R.Behavior (Maybe DatabaseKey),
    bSelectionItem :: R.Behavior (Maybe DatabaseKey),
    bSelectionLoan :: R.Behavior (Maybe DatabaseKey),
    bFilterUser :: R.Behavior String,
    bFilterItem :: R.Behavior String,
    bFilterLoan :: R.Behavior String,
    bModalState :: R.Behavior Bool
  }

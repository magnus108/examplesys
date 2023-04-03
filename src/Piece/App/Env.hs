{-# LANGUAGE DataKinds #-}

module Piece.App.Env
  ( Env,
  )
where

import qualified Graphics.UI.Threepenny.Core
import Piece.CakeSlayer.Has (Field (..), Has)
import Piece.Core.Item (Item)
import Piece.Core.Loan (Loan)
import Piece.Core.User (User)
import Piece.Db.Db (Database, DatabaseKey)

data Env (m :: Type -> Type) = Env
  { loanEnv :: LoanEnv
  }
  deriving (Has LoanEnv) via Field "loanEnv" (Env m)

data LoanEnv = LoanEnv
  { bDatabaseLoan :: Behavior (Database Loan),
    bSelectionUser :: Behavior (Maybe DatabaseKey),
    bSelectionItem :: Behavior (Maybe DatabaseKey),
    bSelectionLoan :: Behavior (Maybe DatabaseKey),
    bFilterUser :: Behavior String,
    bFilterItem :: Behavior String,
    bModalState :: Behavior Bool
  }

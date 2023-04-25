{-# LANGUAGE DataKinds #-}

module Piece.App.Env
  ( Env (..),
    LoanEnv (..),
    TabEnv (..),
    TimeEnv (..),
    RoleEnv (..),
    UserEnv (..),
    WithLoanEnv,
    WithTabEnv,
    WithTimeEnv,
    WithRoleEnv,
    WithUserEnv,
  )
where

import qualified Data.Map as Map
import qualified Data.Time.LocalTime as Time
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.CakeSlayer.Has (Field (..), Has)
import Piece.Core.Loan (Loan)
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Tab as Tab
import qualified Piece.Core.User as User
import Piece.Db.Db (Database, DatabaseKey)
import qualified Reactive.Threepenny as R

data Env (m :: Type -> Type) = Env
  { loanEnv :: LoanEnv,
    tabEnv :: TabEnv,
    timeEnv :: TimeEnv,
    userEnv :: UserEnv,
    roleEnv :: RoleEnv
  }
  deriving (Has LoanEnv) via Field "loanEnv" (Env m)
  deriving (Has TabEnv) via Field "tabEnv" (Env m)
  deriving (Has TimeEnv) via Field "timeEnv" (Env m)
  deriving (Has UserEnv) via Field "userEnv" (Env m)
  deriving (Has RoleEnv) via Field "roleEnv" (Env m)

type WithLoanEnv env m = (MonadReader env m, Has LoanEnv env)

type WithTabEnv env m = (MonadReader env m, Has TabEnv env)

type WithTimeEnv env m = (MonadReader env m, Has TimeEnv env)

type WithUserEnv env m = (MonadReader env m, Has UserEnv env)

type WithRoleEnv env m = (MonadReader env m, Has RoleEnv env)

data TabEnv = TabEnv
  { bDatabaseTab :: R.Behavior (Database Tab.Tab),
    bViewMapTab :: R.Behavior (Map.Map DatabaseKey (UI.UI UI.Element))
  }

data TimeEnv = TimeEnv
  { bTime :: R.Behavior Time.ZonedTime
  }

data UserEnv = UserEnv
  { bDatabaseUser :: R.Behavior (Database User.User)
  }

data RoleEnv = RoleEnv
  { bDatabaseRole :: R.Behavior (Database Role.Role)
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

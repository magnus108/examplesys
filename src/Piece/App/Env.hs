{-# LANGUAGE DataKinds #-}

module Piece.App.Env
  ( Env (..),
    LoanEnv (..),
    ItemEnv (..),
    TabEnv (..),
    TokenEnv (..),
    TimeEnv (..),
    RoleEnv (..),
    PrivilegeEnv (..),
    WithItemEnv,
    WithLoanEnv,
    WithTabEnv,
    WithTimeEnv,
    WithRoleEnv,
    WithUserEnv,
    WithPrivilegeEnv,
    WithTokenEnv,
  )
where

import qualified Data.Time.Clock as Time
import qualified Piece.App.UserEnv as UserEnv
import Piece.CakeSlayer.Has (Field (..), Has)
import qualified Piece.Core.Item as Item
import qualified Piece.Core.ItemCreateForm as ItemCreateForm
import qualified Piece.Core.ItemDeleteForm as ItemDeleteForm
import Piece.Core.Loan (Loan)
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Tab as Tab
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import Piece.Db.Db (Database, DatabaseKey)
import qualified Reactive.Threepenny as R

data Env (m :: Type -> Type) = Env
  { loanEnv :: LoanEnv,
    itemEnv :: ItemEnv,
    tabEnv :: TabEnv,
    timeEnv :: TimeEnv,
    userEnv :: UserEnv.UserEnv,
    roleEnv :: RoleEnv,
    tokenEnv :: TokenEnv,
    privilegeEnv :: PrivilegeEnv
  }
  deriving (Has ItemEnv) via Field "itemEnv" (Env m)
  deriving (Has LoanEnv) via Field "loanEnv" (Env m)
  deriving (Has TabEnv) via Field "tabEnv" (Env m)
  deriving (Has TokenEnv) via Field "tokenEnv" (Env m)
  deriving (Has TimeEnv) via Field "timeEnv" (Env m)
  deriving (Has UserEnv.UserEnv) via Field "userEnv" (Env m)
  deriving (Has RoleEnv) via Field "roleEnv" (Env m)
  deriving (Has PrivilegeEnv) via Field "privilegeEnv" (Env m)

type WithLoanEnv env m = (MonadReader env m, Has LoanEnv env)

type WithItemEnv env m = (MonadReader env m, Has ItemEnv env)

type WithTabEnv env m = (MonadReader env m, Has TabEnv env)

type WithTimeEnv env m = (MonadReader env m, Has TimeEnv env)

type WithUserEnv env m = (MonadReader env m, Has UserEnv.UserEnv env)

type WithRoleEnv env m = (MonadReader env m, Has RoleEnv env)

type WithPrivilegeEnv env m = (MonadReader env m, Has PrivilegeEnv env)

type WithTokenEnv env m = (MonadReader env m, Has TokenEnv env)

data TabEnv = TabEnv
  { bDatabaseTab :: R.Behavior (Database Tab.Tab),
    bSelectionTab :: R.Behavior (Maybe DatabaseKey)
  }

data TokenEnv = TokenEnv
  { bDatabaseToken :: R.Behavior (Database Token.Token),
    bSelectionToken :: R.Behavior (Maybe DatabaseKey),
    bTTL :: R.Behavior (Maybe Time.NominalDiffTime)
  }

data TimeEnv = TimeEnv
  { bTime :: R.Behavior Time.Time
  }

data RoleEnv = RoleEnv
  { bDatabaseRole :: R.Behavior (Database Role.Role)
  }

data PrivilegeEnv = PrivilegeEnv
  { bDatabasePrivilege :: R.Behavior (Database Privilege.Privilege)
  }

data LoanEnv = LoanEnv
  { bDatabaseLoan :: R.Behavior (Database Loan),
    bSelectionUser :: R.Behavior (Maybe DatabaseKey),
    bSelectionItem :: R.Behavior (Maybe DatabaseKey),
    bSelectionLoan :: R.Behavior (Maybe DatabaseKey),
    bFilterUser :: R.Behavior String,
    _bFilterItem :: R.Behavior String,
    bFilterLoan :: R.Behavior String,
    bModalState :: R.Behavior Bool
  }

data ItemEnv = ItemEnv
  { bDatabaseItem :: R.Behavior (Database Item.Item),
    bSelectItem :: R.Behavior (Maybe DatabaseKey),
    bFilterItem :: R.Behavior String,
    bItemDeleteForm :: R.Behavior ItemDeleteForm.Item,
    bItemCreateForm :: R.Behavior ItemCreateForm.Item
  }

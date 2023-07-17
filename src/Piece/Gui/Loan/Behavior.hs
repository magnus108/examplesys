module Piece.Gui.Loan.Behavior
  ( items,
    users,
  )
where

import qualified Data.Barbie as Barbie
import qualified Data.Generic.HKD as HKD
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.User as User
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Item as DbItem
import qualified Piece.Db.Loan as DbLoan
import qualified Piece.Db.User as DbUser
import qualified Reactive.Threepenny as R

items :: (Env.WithLoanEnv env m, Env.WithItemEnv env m) => m (R.Behavior [Db.DatabaseKey])
items = do
  itemEnv <- Has.grab @Env.ItemEnv
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseItem = Env.bDatabaseItem itemEnv
  let bFilterItem = isPrefixOf <$> Env.bLoanCreateItemFilter loanEnv
  bShowItem <- DbItem.showItem
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterItem
      <*> bShowItem
      <*> bDatabaseItem

users :: (Env.WithLoanEnv env m, Env.WithUserEnv env m) => m (R.Behavior [Db.DatabaseKey])
users = do
  userEnv <- Has.grab @UserEnv.UserEnv
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  let bFilterUser = isPrefixOf <$> Env.bLoanCreateUserFilter loanEnv
  bShowUser <- DbUser.showUser
  return $
    (\p display -> filter (p . display) . Db.keys)
      <$> bFilterUser
      <*> bShowUser
      <*> bDatabaseUser

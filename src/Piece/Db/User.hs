{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Piece.Db.User
  ( lookup,
    getRoles,
    create,
    isConfig,
    isConfig2,
    edit,
    bListBox,
    showUser,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Barbie
import Data.Functor.Barbie
import Data.Functor.Product
import qualified Data.Functor.Product as Product
import Data.Generic.HKD
import Data.List.Split (splitOn)
import Data.Text (pack, unpack)
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Core.UserEditForm as UserEditForm
import qualified Piece.Db.Db as Db
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import Prelude hiding (Product)

lookup :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe User.User))
lookup = do
  userEnv <- Has.grab @UserEnv.UserEnv
  return $ flip Db.lookup <$> UserEnv.bDatabaseUser userEnv

showUser :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> String))
showUser = do
  bLookup <- lookup
  return $ (maybe "" User.name .) <$> bLookup

getRoles :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Int]))
getRoles = do
  bLookup <- lookup
  return $ (maybe [] User.roles .) <$> bLookup

create :: MonadIO m => UserCreateForm.User -> m (Maybe User.User)
create form = do
  hala <- liftIO $ bsequence $ bmap (\(Product.Pair conf x) -> UserCreateForm.constructData x) form
  return $ construct @Maybe hala

isConfig :: UserCreateForm.User -> Bool
isConfig form = getAny $ bfoldMap (\(Product.Pair (Const (UserCreateForm.Config conf)) x) -> Any conf) form

isConfig2 :: UserEditForm.User -> Bool
isConfig2 form = getAny $ bfoldMap (\(Product.Pair (Product.Pair (Const (UserCreateForm.Config conf)) _) x) -> Any conf) form

edit :: MonadIO m => UserEditForm.User -> m (Maybe User.User)
edit form = do
  hala <- liftIO $ bsequence $ bmap (\(Product.Pair conf x) -> UserCreateForm.constructData x) form
  return $ construct @Maybe hala

bListBox :: (Env.WithUserEnv env m) => m (R.Behavior [Db.DatabaseKey])
bListBox = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  return $ Db.keys <$> bDatabaseUser

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Piece.Db.User
  ( lookup,
    getRoles,
    create,
    edit,
    bListBox,
    showUser,
    formEdit,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Functor.Product
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
  let formName = UserCreateForm.name form
      formPassword = UserCreateForm.password form
      formAdmin = UserCreateForm.admin form
      roles = if formAdmin then [0, 1, 2] else [0, 1]
  password <- Password.mkPasswordHash formPassword
  return $ case password of
    Nothing -> Nothing
    Just p -> Just (User.user formName p roles)

edit :: MonadIO m => User.User -> UserEditForm.User -> m (Maybe User.User)
edit user form = do
  let formName = form ^. field @"name"
      formPassword = form ^. field @"password"
      formRoles = form ^. field @"roles"

  let formName' = getConst formName
      formPassword' = getConst formPassword
      formRoles' = mapMaybe readMaybe (splitOn "," (getConst formRoles))

  password <- Password.mkPasswordHash (Password.PasswordPlainText (pack formPassword'))

  return $ case password of
    Nothing -> Nothing
    Just p -> Just (User.user formName' p formRoles')

formEdit :: User.User -> Maybe UserEditForm.User
formEdit user =
  let name = Const $ User.name user
      password = Const ""
      roles = Const ""
   in Just (UserEditForm.user name password roles)

bListBox :: (Env.WithUserEnv env m) => m (R.Behavior [Db.DatabaseKey])
bListBox = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  return $ Db.keys <$> bDatabaseUser

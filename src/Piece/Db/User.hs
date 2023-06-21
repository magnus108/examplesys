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
import Data.Barbie
import Data.Functor.Barbie
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
  undefined

--  password <- mapM (Password.mkPasswordHash . Password.PasswordPlainText . pack) (UserCreateForm.toPassword form)
-- return $ User.user <$> Just (UserCreateForm.toName form) <*> join password <*> UserCreateForm.toRoles form

create2 :: MonadIO m => UserCreateForm.User -> HKD User.User (Compose m Maybe)
create2 form =
  -- (unzip1, unzip2) = bunzip form
  undefined

-- bmap f form

-- f :: (MonadIO m, UserCreateForm.MyHashable m (Product UserCreateForm.FormInput UserCreateForm.Hash) a) => Product UserCreateForm.FormInput UserCreateForm.Hash a -> Compose m Maybe a

{-
f :: (AllBF UserCreateForm.MyHashable UserCreateForm.Hash UserCreateForm.User, ConstraintsB UserCreateForm.User) => UserCreateForm.FormInput a -> UserCreateForm.Hash a -> Compose IO Maybe a
f formInput hash = case formInput of
  Nothing -> case getCompose hash of
    Nothing -> Compose $ return $ Nothing
    Just y -> UserCreateForm.hash hash
  Just x -> Compose $ return $ formInput
  -}

-- gg :: (AllBF UserCreateForm.MyHashable f b, ConstraintsB b) => b (Product UserCreateForm.FormInput UserCreateForm.Hash) -> b (Const String)
-- gg = bmapC @UserCreateForm.MyHashable showField
-- where
--  showField :: forall a. UserCreateForm.MyHashable a => Product UserCreateForm.FormInput UserCreateForm.Hash a -> Const String a
-- showField x = undefined -- Const (show a)

edit :: MonadIO m => User.User -> UserEditForm.User -> m (Maybe User.User) -- fromForm
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

formEdit :: User.User -> Maybe UserEditForm.User -- toForm
formEdit user =
  let name = Const $ User.name user
      password = Const ""
      roles = Const $ intercalate "," . fmap show $ User.roles user
   in Just (UserEditForm.user name password roles)

bListBox :: (Env.WithUserEnv env m) => m (R.Behavior [Db.DatabaseKey])
bListBox = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  return $ Db.keys <$> bDatabaseUser

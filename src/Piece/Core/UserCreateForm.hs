{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserCreateForm
  ( User,
    form,
    form2,
    getFormData,
    constructData,
    toName,
    toRoles,
    toPassword,
    toRoles,
    Config (..),
    UserForm,
    UserConfig (..),
    FormDataExpr (..),
    FormData (..),
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import qualified Data.Functor.Product as Product
import Data.Generic.HKD
import Data.Text hiding (elem)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User

type UserConfig = HKD User.User (Const Config)

type UserForm = HKD User.User FormDataExpr

type User = HKD User.User (Product.Product (Const Config) FormDataExpr)

data Config = Config {enabled :: Bool}

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String
  PasswordExpr :: String -> FormDataExpr Password.PasswordHash
  BoolExpr :: Bool -> FormDataExpr [Int]

type family FormData a where
  FormData (FormDataExpr String) = String
  FormData (FormDataExpr [Int]) = Bool
  FormData (FormDataExpr Password.PasswordHash) = String

getFormData :: FormDataExpr a -> FormData (FormDataExpr a)
getFormData (StringExpr param) = param
getFormData (PasswordExpr param) = param
getFormData (BoolExpr param) = param

constructData :: FormDataExpr a -> Compose IO Maybe a
constructData (StringExpr param) = Compose $ return $ Just param
constructData (PasswordExpr param) = Compose $ Password.mkPasswordHash . Password.PasswordPlainText . pack $ param
constructData (BoolExpr param) = Compose $ return $ Just $ if param then [3] else []

form :: String -> String -> Bool -> User
form name password admin =
  build @User.User
    (Product.Pair (Const (Config True)) (StringExpr name))
    (Product.Pair (Const (Config True)) (PasswordExpr password))
    (Product.Pair (Const (Config True)) (BoolExpr admin))

form2 :: String -> String -> Bool -> User
form2 name password admin =
  build @User.User
    (Product.Pair (Const (Config False)) (StringExpr name))
    (Product.Pair (Const (Config False)) (PasswordExpr password))
    (Product.Pair (Const (Config False)) (BoolExpr admin))

toName :: User -> (Config, String)
toName user =
  let (Product.Pair conf formName) = user ^. field @"name"
      formData = getFormData formName
   in (getConst conf, formData)

toPassword :: User -> (Config, String)
toPassword user =
  let (Product.Pair conf formName) = user ^. field @"password"
      formData = getFormData formName
   in (getConst conf, formData)

toRoles :: User -> (Config, Bool)
toRoles user =
  let (Product.Pair conf formName) = user ^. field @"roles"
      formData = getFormData formName
   in (getConst conf, formData)

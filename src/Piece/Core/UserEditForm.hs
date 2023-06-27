{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserEditForm
  ( User,
    form,
    form2,
    fromUser,
    toRoles,
    toName,
    toPassword,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Product (Product)
import qualified Data.Functor.Product as Product
import Data.Generic.HKD
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as Form
import Prelude hiding (Product)

type User = HKD User.User (Product.Product (Product.Product (Const Form.Config) Maybe) Form.FormDataExpr)

form :: String -> String -> Bool -> User
form name password admin =
  build @User.User
    (Product.Pair (Product.Pair (Const (Form.Config False)) Nothing) (Form.StringExpr name))
    (Product.Pair (Product.Pair (Const (Form.Config False)) Nothing) (Form.PasswordExpr password))
    (Product.Pair (Product.Pair (Const (Form.Config False)) Nothing) (Form.BoolExpr admin))

form2 :: User -> String -> String -> Bool -> User
form2 user name password admin =
  build @User.User
    (Product.Pair (Product.Pair (Const (Form.Config True)) Nothing) (Form.StringExpr name))
    (Product.Pair (Product.Pair (Const (Form.Config True)) Nothing) (Form.PasswordExpr password))
    (Product.Pair (Product.Pair (Const (Form.Config True)) Nothing) (Form.BoolExpr admin))

form3 :: (Maybe String, String) -> (Maybe Password.PasswordHash, String) -> (Maybe [Int], Bool) -> User
form3 (realName, name) (realPassword, password) (roles, admin) =
  build @User.User
    (Product.Pair (Product.Pair (Const (Form.Config True)) realName) (Form.StringExpr name))
    (Product.Pair (Product.Pair (Const (Form.Config True)) realPassword) (Form.PasswordExpr password))
    (Product.Pair (Product.Pair (Const (Form.Config True)) roles) (Form.BoolExpr admin))

fromUser :: User.User -> User
fromUser user = form3 (Just (User.name user), User.name user) (Just (User.password user), "") (Just (User.roles user), False)

toName :: User -> (Form.Config, String)
toName user =
  let (Product.Pair (Product.Pair conf _) formName) = user ^. field @"name"
      formData = Form.getFormData formName
   in (getConst conf, formData)

toPassword :: User -> (Form.Config, String)
toPassword user =
  let (Product.Pair (Product.Pair conf _) formName) = user ^. field @"password"
      formData = Form.getFormData formName
   in (getConst conf, formData)

toRoles :: User -> (Form.Config, Bool)
toRoles user =
  let (Product.Pair (Product.Pair conf _) formName) = user ^. field @"roles"
      formData = Form.getFormData formName
   in (getConst conf, formData)

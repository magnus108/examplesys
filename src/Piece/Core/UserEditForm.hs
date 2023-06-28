{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserEditForm
  ( User,
    form,
    emptyForm,
    fromUser,
    constructData,
    toName,
    toPassword,
    toRoles,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Product (Product)
import qualified Data.Functor.Product as Product
import Data.Generic.HKD
import Data.Text hiding (any, elem, filter, null)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as Form
import Prelude hiding (Product)

type User = HKD User.User (Product.Product (Product.Product (Const Form.Config) Form.FormDataExpr) Maybe)

emptyForm :: User
emptyForm = form "" "" False

form :: String -> String -> Bool -> User
form name password admin =
  bmap (\x -> Product.Pair (Product.Pair (Const (Form.Config False)) x) Nothing) $ -- her smider alt væk
    build @User.User
      (Form.StringExpr name)
      (Form.PasswordExpr password)
      (Form.BoolExpr admin)

toName :: User -> (Form.Config, String)
toName user =
  let (Product.Pair (Product.Pair conf formName) fallback) = user ^. field @"name"
      formData = Form.getFormData formName
   in (getConst conf, formData)

toPassword :: User -> (Form.Config, String)
toPassword user =
  let (Product.Pair (Product.Pair conf formName) fallback) = user ^. field @"password"
      formData = Form.getFormData formName
   in (getConst conf, formData)

toRoles :: User -> (Form.Config, Bool)
toRoles user =
  let (Product.Pair (Product.Pair conf formName) fallback) = user ^. field @"roles"
      formData = Form.getFormData formName
   in (getConst conf, formData)

fromUser :: User -> User.User -> User
fromUser user fallback =
  build @User.User
    (Product.Pair (Product.Pair (Const (Form.Config True)) (Form.StringExpr (User.name fallback))) (Just $ User.name fallback))
    (Product.Pair (Product.Pair (Const (Form.Config True)) (Form.PasswordExpr "")) (Just $ User.password fallback))
    (Product.Pair (Product.Pair (Const (Form.Config True)) (Form.BoolExpr (3 `elem` (User.roles fallback)))) (Just $ User.roles fallback))

constructData :: Maybe a -> Form.FormDataExpr a -> Compose IO Maybe a
constructData x (Form.StringExpr param) = if null param then Compose $ return x else Compose $ return $ Just param
constructData x (Form.PasswordExpr param) = if null param then Compose $ return x else Compose $ Password.mkPasswordHash . Password.PasswordPlainText . pack $ param
constructData x (Form.BoolExpr param) = Compose $ return $ Just [] -- \$ if param then ordNub . ((:) 3) <$> x else filter (/= 3) <$> x

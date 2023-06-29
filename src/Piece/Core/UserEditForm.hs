{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserEditForm
  ( User,
    lola,
    form,
    emptyForm,
    fromUser,
    constructData,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Barbie
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

fromUser :: User -> HKD User.User Identity -> User
fromUser user fallback = bzipWith lol user fallback

lol :: Product.Product (Product.Product (Const Form.Config) Form.FormDataExpr) Maybe a -> Identity a -> Product.Product (Product.Product (Const Form.Config) Form.FormDataExpr) Maybe a
lol (Product.Pair (Product.Pair config x) _) y = Product.Pair (Product.Pair (Const (Form.Config True)) (lol2 x y)) (Just $ runIdentity y)

lol2 :: Form.FormDataExpr a -> Identity a -> Form.FormDataExpr a
lol2 (Form.StringExpr param) x = Form.StringExpr (runIdentity x)
lol2 (Form.PasswordExpr param) x = Form.PasswordExpr ""
lol2 (Form.BoolExpr param) x = Form.BoolExpr (3 `elem` runIdentity x)

lola ::
  Product.Product (Product.Product (Const Form.Config) Form.FormDataExpr) Maybe a ->
  Product.Product (Product.Product (Const Form.Config) Form.FormDataExpr) Maybe a ->
  Product.Product (Product.Product (Const Form.Config) Form.FormDataExpr) Maybe a
lola (Product.Pair (Product.Pair config x) p) (Product.Pair (Product.Pair _ y) _) = Product.Pair (Product.Pair config y) p

constructData :: Maybe a -> Form.FormDataExpr a -> Compose IO Maybe a
constructData x (Form.StringExpr param) = if null param then Compose $ return x else Compose $ return $ Just param
constructData x (Form.PasswordExpr param) = if null param then Compose $ return x else Compose $ Password.mkPasswordHash . Password.PasswordPlainText . pack $ param
constructData x (Form.BoolExpr param) = Compose $ return $ Just [] -- \$ if param then ordNub . ((:) 3) <$> x else filter (/= 3) <$> x

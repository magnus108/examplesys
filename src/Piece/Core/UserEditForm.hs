module Piece.Core.UserEditForm
  ( User,
    form,
    form2,
    fromUser,
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

type User = HKD User.User (Product.Product (Const Form.Config) Form.FormDataExpr)

form :: String -> String -> Bool -> User
form name password admin =
  build @User.User
    (Product.Pair (Const (Form.Config False)) (Form.StringExpr name))
    (Product.Pair (Const (Form.Config False)) (Form.PasswordExpr password))
    (Product.Pair (Const (Form.Config False)) (Form.BoolExpr admin))

form2 :: String -> String -> Bool -> User
form2 name password admin =
  build @User.User
    (Product.Pair (Const (Form.Config True)) (Form.StringExpr name))
    (Product.Pair (Const (Form.Config True)) (Form.PasswordExpr password))
    (Product.Pair (Const (Form.Config True)) (Form.BoolExpr admin))

fromUser :: User.User -> User
fromUser user = form2 (User.name user) ("") (False)

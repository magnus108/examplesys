module Piece.Core.UserEditForm
  ( User,
    user,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Product (Product)
import Data.Generic.HKD
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import Prelude hiding (Product)

type User = HKD User.User (Const String)

user :: f String -> f Password.PasswordHash -> f [Int] -> HKD User.User f
user = build @User.User

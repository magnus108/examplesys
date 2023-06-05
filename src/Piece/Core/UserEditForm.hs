module Piece.Core.UserEditForm
  ( User,
    user,
    Mk (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Product (Product)
import Data.Generic.HKD
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import Prelude hiding (Product)

type User = HKD User.User F

data Mk a = Mk
  { form :: String,
    read :: Maybe a,
    write :: IO a
  }
  deriving (Functor)

type F = Const String

user :: F String -> F Password.PasswordHash -> F [Int] -> User
user = build @User.User

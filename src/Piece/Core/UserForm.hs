module Piece.Core.UserForm
  ( User,
    name,
    user,
    password,
    admin,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.Role as Role

data User = User
  { name :: String,
    password :: Password.PasswordPlainText,
    admin :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

user :: String -> Password.PasswordPlainText -> Bool -> User
user = User

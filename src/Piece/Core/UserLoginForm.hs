module Piece.Core.UserLoginForm
  ( User,
    name,
    user,
    password,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Piece.CakeSlayer.Password as Password

data User = User
  { name :: String,
    password :: Password.PasswordPlainText
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

user :: String -> Password.PasswordPlainText -> User
user = User

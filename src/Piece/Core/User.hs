module Piece.Core.User
  ( User,
    name,
    user,
    password,
    roles,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Piece.CakeSlayer.Password as Password

data User = User
  { name :: String,
    password :: Password.PasswordHash,
    roles :: [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

user :: String -> Password.PasswordHash -> [Int] -> User
user = User

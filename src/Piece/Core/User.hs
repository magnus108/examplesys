module Piece.Core.User
  ( User,
    name,
    user,
    password,
    roles,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Piece.Core.Role as Role

data User = User
  { name :: String,
    password :: String,
    roles :: [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

user :: String -> String -> [Int] -> User
user = User

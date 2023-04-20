module Piece.Core.User
  ( User,
    name,
    user,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype User = User
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

user :: String -> User
user = User

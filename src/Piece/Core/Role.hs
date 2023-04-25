module Piece.Core.Role
  ( Role,
    role,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Role = Role
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

role :: String -> Role
role = Role

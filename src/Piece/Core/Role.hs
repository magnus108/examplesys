module Piece.Core.Role
  ( Role,
    role,
    privilege,
  )
where

import Data.Aeson (FromJSON, ToJSON)

data Role = Role
  { name :: String,
    privilege :: [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

role :: String -> [Int] -> Role
role = Role

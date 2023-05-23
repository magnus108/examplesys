module Piece.Core.Privilege
  ( Privilege,
    privilege,
    name,
    contains,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra (disjoint)

newtype Privilege = Privilege
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

privilege :: String -> Privilege
privilege = Privilege

contains :: [Privilege] -> [Privilege] -> Bool
contains xs ys = not (disjoint xs ys)

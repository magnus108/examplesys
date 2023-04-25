module Piece.Core.Privilege
  ( Privilege,
    privilege,
    name,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Privilege = Privilege
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

privilege :: String -> Privilege
privilege = Privilege

module Piece.Core.Item
  ( Item,
    name,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Item = Item
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
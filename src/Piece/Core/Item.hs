module Piece.Core.Item
    ( Item (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)

newtype Item = Item
    { itemText :: Text
    } deriving stock (Show, Eq, Generic)
      deriving (ToJSON, FromJSON)

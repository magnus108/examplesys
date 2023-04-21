module Piece.Core.Tab
  ( Tab,
    name,
    tab,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Tab = Tab
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

tab :: String -> Tab
tab = Tab

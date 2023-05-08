module Piece.Core.Tab
  ( Tab,
    name,
    privilege,
    tab,
  )
where

import Data.Aeson (FromJSON, ToJSON)

data Tab = Tab
  { name :: String,
    privilege :: [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

tab :: String -> [Int] -> Tab
tab = Tab

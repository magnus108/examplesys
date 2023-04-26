module Piece.Core.Time
  ( Time,
    time,
    unTime,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Time = Time
  { unTime :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

time :: String -> Time
time = Time

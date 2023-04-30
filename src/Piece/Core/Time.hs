module Piece.Core.Time
  ( Time,
    time,
    unTime,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)

newtype Time = Time
  { unTime :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

time :: UTCTime -> Time
time = Time

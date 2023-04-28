module Piece.Core.Time
  ( Time,
    time,
    unTime,
    Tom (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (TimeLocale (wDays), UTCTime)

newtype Time = Time
  { unTime :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype Tom = Tom
  { unTom :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

time :: String -> Time
time = Time

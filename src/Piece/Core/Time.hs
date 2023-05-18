module Piece.Core.Time
  ( Time,
    time,
    unTime,
    diffTime,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import qualified Data.Time.Clock as Time

newtype Time = Time
  { unTime :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

time :: UTCTime -> Time
time = Time

diffTime :: Time -> Time -> Time.NominalDiffTime
diffTime x y = Time.diffUTCTime (unTime x) (unTime y)

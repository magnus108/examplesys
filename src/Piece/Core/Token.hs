module Piece.Core.Token
  ( Token,
    token,
    time,
    user,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Piece.Core.Time as Time

data Token = Token
  { user :: Int,
    time :: Time.Time
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

token :: Int -> Time.Time -> Token
token = Token

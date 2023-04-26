module Piece.Core.Token
  ( Token,
    token,
    name,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Piece.Core.Time as Time

data Token = Token
  { name :: String,
    time :: Time.Time
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)

token :: String -> Time.Time -> Token
token = Token

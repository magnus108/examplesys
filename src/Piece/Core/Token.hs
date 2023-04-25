module Piece.Core.Token
  ( Token,
    token,
    name,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Token = Token
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

token :: String -> Token
token = Token

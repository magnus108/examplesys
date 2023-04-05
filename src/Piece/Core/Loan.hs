module Piece.Core.Loan
  ( Loan (..),
    name,
  )
where

import Data.Aeson (FromJSON, ToJSON)

newtype Loan = Loan
  { name :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

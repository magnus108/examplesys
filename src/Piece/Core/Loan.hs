module Piece.Core.Loan
  ( Loan,
    user,
    item,
  )
where

import Data.Aeson (FromJSON, ToJSON)

data Loan = Loan
  { user :: Int,
    item :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

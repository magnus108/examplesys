module Piece.Core.ItemDeleteForm
  ( Item,
  )
where

import qualified Data.Generic.HKD as HKD
import qualified Piece.Core.Item as Item

type Item = HKD.HKD Item.Item (Maybe)

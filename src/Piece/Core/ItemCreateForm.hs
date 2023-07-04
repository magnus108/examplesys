module Piece.Core.ItemCreateForm
  ( Item,
  )
where

import qualified Data.Generic.HKD as HKD
import qualified Piece.Core.Form.FormDataExpr as Form
import qualified Piece.Core.Item as Item

type Item = HKD.HKD Item.Item (Compose Maybe Form.FormDataExpr)

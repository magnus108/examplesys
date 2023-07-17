module Piece.Core.ItemEditForm
  ( Item,
    Form (..),
  )
where

import qualified Data.Generic.HKD as HKD
import qualified Piece.Core.Form.FormDataExpr as Form
import qualified Piece.Core.Item as Item

type Item = HKD.HKD Item.Item Form

data Form a = Form
  { from :: Maybe (Form.FormDataExpr a),
    to :: a -> Form.FormDataExpr a
  }

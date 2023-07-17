module Piece.Core.LoanCreateForm
  ( Loan,
  )
where

import qualified Data.Generic.HKD as HKD
import qualified Piece.Core.Form.FormDataExpr as Form
import qualified Piece.Core.Loan as Loan

type Loan = HKD.HKD Loan.Loan Form.Form

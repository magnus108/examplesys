module Piece.Core.Loan.Tests
  ( tests,
  )
where

import Test.Tasty
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Core.Loan.Tests" $
    concat
      [ fromAssertions
          ""
          []
      ]

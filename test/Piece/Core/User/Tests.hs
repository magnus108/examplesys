module Piece.Core.User.Tests
  ( tests,
  )
where

import Test.Tasty
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Core.User.Tests" $
    concat
      [ fromAssertions
          ""
          []
      ]

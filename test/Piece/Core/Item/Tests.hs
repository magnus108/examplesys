module Piece.Core.Item.Tests
  ( tests,
  )
where

import Test.Tasty
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Core.Item.Tests" $
    concat
      [ fromAssertions
          ""
          []
      ]

module Piece.Core.Item.Tests
  ( tests,
  )
where

import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Core.Item.Tests" $
    concat
      [ --fromAssertions
        --  "forest"
         -- [ forest @=? (T.trie project & F.fromTrie)
          --]
      ]

{-# LANGUAGE DataKinds #-}

module Piece.Db.Db.Tests
  ( tests,
  )
where

import qualified Piece.Db.Db as Db
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Db.Db.Tests" $
    concat
      [ fromAssertions
          "delete"
          [ ([] :: [(Db.DatabaseKey, Int)]) @=? Db.toList (Db.delete 0 Db.empty),
            [] @=? Db.toList (Db.delete 0 (Db.create "a" Db.empty))
          ],
        fromAssertions
          "elems"
          [ ["a"] @=? Db.elems (Db.create "a" Db.empty)
          ],
        fromAssertions
          "create"
          [ [(0, "a")] @=? Db.toList (Db.create "a" Db.empty)
          ],
        fromAssertions
          "empty"
          [ ([] :: [(Db.DatabaseKey, Int)]) @=? Db.toList Db.empty
          ],
        fromAssertions
          "delete"
          [ [] @=? Db.toList (Db.delete 0 (Db.create "a" Db.empty))
          ],
        fromAssertions
          "lookup"
          [ Just "a" @=? Db.lookup 0 (Db.create "a" Db.empty),
            (Nothing :: Maybe String) @=? Db.lookup 0 Db.empty
          ],
        fromAssertions
          "keys"
          [ [0] @=? Db.keys (Db.create "a" Db.empty)
          ],
        fromAssertions
          "toList"
          [ [(0, "a")] @=? Db.toList (Db.create "a" Db.empty)
          ],
        fromAssertions
          "update"
          [ [(0, "b")] @=? Db.toList (Db.update 0 "b" (Db.create "a" Db.empty)),
            [(0, "a"), (1, "b")] @=? Db.toList (Db.update 1 "b" (Db.create "a" Db.empty))
          ]
      ]

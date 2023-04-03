module Main
  ( main,
  )
where

import qualified Piece.Core.Item.Tests
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Piece.Core.Item.Tests.tests
      ]

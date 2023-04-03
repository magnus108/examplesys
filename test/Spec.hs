module Main
  ( main,
  )
where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ 
        --Blog.Utils.Link.Tests.tests
      ]

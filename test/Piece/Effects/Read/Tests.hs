{-# LANGUAGE DataKinds #-}

module Piece.Effects.Read.Tests
  ( tests,
  )
where

import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Read as Read
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Mock
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Effect.Read.Tests" $
    concat
      [ fromAssertions
          "read"
          [ test
          ]
      ]
  where
    test = do
      let value = Db.create (Loan.loan "3") (Db.create (Loan.loan "2") (Db.create (Loan.loan "1") Db.empty))
      result <- runMockApp $ do
        x <- Read.read (Config.datastoreLoan mockConfig)
        return x
      (Right value) @=? result

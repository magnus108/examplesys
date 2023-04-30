{-# LANGUAGE DataKinds #-}

module Piece.Db.Loan.Tests
  ( tests,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Loan as DBLoan
import qualified Reactive.Threepenny as R
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Mock
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Db.Loan.Tests" $
    concat
      [ fromAssertions
          "lookup"
          [ lookupLoan
          ]
      ]
  where
    lookupLoan = do
      lookup <- runMockApp $ do
        x <- DBLoan.lookup
        R.currentValue (x ?? 0)
      let value = Just (Loan.loan "1")
      value @=? lookup

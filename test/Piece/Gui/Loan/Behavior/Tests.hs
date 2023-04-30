{-# LANGUAGE DataKinds #-}

module Piece.Gui.Loan.Behavior.Tests
  ( tests,
  )
where

import qualified Piece.Gui.Loan.Behavior as LoanBehavior
import qualified Reactive.Threepenny as R
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Mock
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Gui.Loan.Behavior.Tests" $
    concat
      [ fromAssertions
          "showLoan"
          [ test,
            test2
          ],
        fromAssertions
          "displayLoan" -- Foreign.JavaScript ?
          [],
        fromAssertions
          "bListBox"
          [test3]
      ]
  where
    test = do
      result <- runMockApp $ do
        showLoan <- LoanBehavior.showLoan
        R.currentValue (showLoan ?? 0)
      let value = "1"
      value @=? result
    test2 = do
      result <- runMockApp $ do
        showLoan <- LoanBehavior.showLoan
        R.currentValue (showLoan ?? 10)
      let value = ""
      value @=? result
    test3 = do
      result <- runMockApp $ do
        bListBox <- LoanBehavior.bListBox
        R.currentValue bListBox
      let value = [0]
      value @=? result

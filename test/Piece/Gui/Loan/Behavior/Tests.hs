{-# LANGUAGE DataKinds #-}

module Piece.Gui.Loan.Behavior.Tests
  ( tests,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Gui.Loan.Behavior as LoanBehavior
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

data MockEnv (m :: Type -> Type) = MockEnv
  { loanEnv :: Env.LoanEnv
  }
  deriving (CakeSlayer.Has Env.LoanEnv) via CakeSlayer.Field "loanEnv" (MockEnv m)

mkMockEnv :: IO (MockEnv IO)
mkMockEnv = do
  bDatabaseLoan <- R.stepper (Db.create (Loan.loan "1") Db.empty) $ Unsafe.head <$> R.unions []
  return $
    MockEnv
      { loanEnv =
          Env.LoanEnv
            { bDatabaseLoan = bDatabaseLoan,
              bSelectionUser = undefined,
              bSelectionItem = undefined,
              bSelectionLoan = undefined,
              bFilterUser = undefined,
              bFilterItem = undefined,
              bFilterLoan = undefined,
              bModalState = undefined
            }
      }

type MockApp = CakeSlayer.App Void (MockEnv IO)

runMockApp :: MockEnv IO -> MockApp a -> IO a
runMockApp = CakeSlayer.runApp

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
      mockEnv <- mkMockEnv
      result <- runMockApp mockEnv $ do
        showLoan <- LoanBehavior.showLoan
        R.currentValue (showLoan ?? 0)
      let value = "1"
      value @=? result
    test2 = do
      mockEnv <- mkMockEnv
      result <- runMockApp mockEnv $ do
        showLoan <- LoanBehavior.showLoan
        R.currentValue (showLoan ?? 10)
      let value = ""
      value @=? result
    test3 = do
      mockEnv <- mkMockEnv
      result <- runMockApp mockEnv $ do
        bListBox <- LoanBehavior.bListBox (pure (const True))
        R.currentValue bListBox
      let value = [0]
      value @=? result

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
import TestSuite.Util

data MockEnv (m :: Type -> Type) = MockEnv
  { loanEnv :: Env.LoanEnv
  }
  deriving (CakeSlayer.Has Env.LoanEnv) via CakeSlayer.Field "loanEnv" (MockEnv m)

mockEnv :: MockEnv IO
mockEnv =
  MockEnv
    { loanEnv =
        Env.LoanEnv
          { bDatabaseLoan = pure (DB.create (Loan.loan "test") DB.empty),
            bSelectionUser = undefined,
            bSelectionItem = undefined,
            bSelectionLoan = undefined,
            bFilterUser = undefined,
            bFilterItem = undefined,
            bFilterLoan = undefined,
            bModalState = undefined
          }
    }

runMockApp :: IO (Maybe Loan.Loan)
runMockApp = CakeSlayer.runApp mockEnv $ do
  x <- DBLoan.lookup
  R.currentValue (x ?? 0)

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
      lookup <- runMockApp
      let value = Just (Loan.loan "test")
      value @=? lookup

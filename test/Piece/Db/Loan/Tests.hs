{-# LANGUAGE DataKinds #-}

module Piece.Db.Loan.Tests
  ( tests,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import Piece.CakeSlayer.Has (Field (..), Has (..), grab)
import Piece.CakeSlayer.Monad (App, runApp)
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Loan as DBLoan
import qualified Reactive.Threepenny as R
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

type MockApp = App () MockEnv

newtype MockEnv = MockEnv
  { loanEnv :: Env.LoanEnv
  }
  deriving (Has Env.LoanEnv) via Field "loanEnv" MockEnv

mockEnv :: MockEnv
mockEnv =
  MockEnv
    { loanEnv =
        Env.LoanEnv
          { bDatabaseLoan = pure (DB.create (Loan.Loan "test") DB.empty),
            eDatabaseLoan = undefined,
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
runMockApp = runApp mockEnv $ do
  x <- DBLoan.lookup
  R.currentValue (x <*> pure 0)

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
      let value = Just (Loan.Loan "test")
      value @=? lookup

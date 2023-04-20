{-# LANGUAGE DataKinds #-}

module Piece.Effects.Read.Tests
  ( tests,
  )
where

import qualified Control.Monad.IO.Unlift as Unlift
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Change as Change
import qualified Piece.Effects.Read as Read
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

-- ALTERNATIVET ER FAKTIS AT SÆTTE EN DB OP GENNEM END CONFIG
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

instance Read.MonadRead MockApp where
  read x = do
    loanEnv <- CakeSlayer.grab @Env.LoanEnv
    let bDatabaseLoan = Env.bDatabaseLoan loanEnv
    R.currentValue bDatabaseLoan

runMockApp :: MockEnv IO -> MockApp a -> IO a
runMockApp = CakeSlayer.runApp

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
      mockEnv <- mkMockEnv
      result <- runMockApp mockEnv $ do
        Read.read ""
      let value = Db.create (Loan.loan "1") Db.empty
      value @=? result

{-# LANGUAGE DataKinds #-}

module Piece.Effects.Write.Tests
  ( tests,
  )
where

import qualified Control.Monad.IO.Unlift as Unlift
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Write as Write
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

-- ALTERNATIVET ER FAKTIS AT SÆTTE EN DB OP GENNEM END CONFIG
data MockEnv (m :: Type -> Type) = MockEnv
  { loanEnv :: Env.LoanEnv,
    loanDBIORef :: IORef (Db.Database Loan.Loan),
    loanDBH :: R.Handler (Db.Database Loan.Loan -> Db.Database Loan.Loan)
  }
  deriving (CakeSlayer.Has Env.LoanEnv) via CakeSlayer.Field "loanEnv" (MockEnv m)
  deriving (CakeSlayer.Has (R.Handler (Db.Database Loan.Loan -> Db.Database Loan.Loan))) via CakeSlayer.Field "loanDBH" (MockEnv m)
  deriving (CakeSlayer.Has (IORef (Db.Database Loan.Loan))) via CakeSlayer.Field "loanDBIORef" (MockEnv m)

mkMockEnv :: IO (MockEnv IO)
mkMockEnv = do
  (e, h) <- R.newEvent
  bDatabaseLoan <- R.accumB Db.empty $ Unsafe.head <$> R.unions [e]
  ioRef <- newIORef Db.empty
  return $
    MockEnv
      { loanDBH = h,
        loanDBIORef = ioRef,
        loanEnv =
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

instance Write.MonadWrite MockApp (Db.Database Loan.Loan) where
  write x s = do
    ioRef <- CakeSlayer.grab @(IORef (Db.Database Loan.Loan))
    writeIORef ioRef s

runMockApp :: MockEnv IO -> MockApp a -> IO a
runMockApp = CakeSlayer.runApp

tests :: TestTree
tests =
  testGroup "Piece.Effect.Write.Tests" $
    concat
      [ fromAssertions
          "write"
          [ test
          ]
      ]
  where
    test = do
      mockEnv <- mkMockEnv
      result <- runMockApp mockEnv $ do
        h <- CakeSlayer.grab @(R.Handler (Db.Database Loan.Loan -> Db.Database Loan.Loan))
        ioRef <- CakeSlayer.grab @(IORef (Db.Database Loan.Loan))
        loanEnv <- CakeSlayer.grab @Env.LoanEnv
        let bDatabaseLoan = Env.bDatabaseLoan loanEnv
        liftIO $ R.onChange bDatabaseLoan $ \s -> runMockApp mockEnv $ Write.write "" s
        liftIO $ h $ Db.create (Loan.loan "1")
        liftIO $ h $ Db.create (Loan.loan "2")
        liftIO $ h $ Db.create (Loan.loan "3")
        readIORef ioRef
      let value = Db.create (Loan.loan "3") (Db.create (Loan.loan "2") (Db.create (Loan.loan "1") Db.empty))
      value @=? result

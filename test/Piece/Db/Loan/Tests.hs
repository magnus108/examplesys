{-# LANGUAGE DataKinds #-}

module Piece.Db.Loan.Tests
  ( tests,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Async
import Control.Exception (try)
import Control.Monad.Fix
import Control.Retry
import Graphics.UI.Threepenny (UI)
import qualified Graphics.UI.Threepenny.Core as UI
import Network.WebSockets
import qualified Piece.App.Env as Env
import Piece.CakeSlayer.Has (Field (..), Has (..), grab)
import Piece.CakeSlayer.Monad (App, runApp)
import qualified Piece.CakeSlayer.Monad as Monad
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
            bSelectionUser = undefined,
            bSelectionItem = undefined,
            bSelectionLoan = undefined,
            bFilterUser = undefined,
            bFilterItem = undefined,
            bFilterLoan = undefined,
            bModalState = undefined
          }
    }

start port mvar = UI.startGUI
  UI.defaultConfig
    { UI.jsPort = Just port
    }
  $ \window -> void $ do
    res <- Monad.runApp mockEnv $ do
      x <- DBLoan.lookup
      R.currentValue (x <*> pure 0)
    putMVar mvar res

run :: Int -> IO (Either SomeException ())
run port = try $ runClient "localhost" port "/websocket" $ \conn -> do
  return ()

runMockApp :: IO (Maybe Loan.Loan)
runMockApp = do
  let port = 8080
  mvar <- newEmptyMVar
  server <- forkIO $ start port mvar
  _ <- retrying retryPolicyDefault (const $ return . isRight) (\_ -> run port)
  result <- readMVar mvar
  killThread server
  return result

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

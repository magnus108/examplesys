{-# LANGUAGE DataKinds #-}

module TestSuite.Mock
  ( runMockApp,
    MockEnv (..),
    mockConfig,
  )
where

import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.Token as Token
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Write as Write

data MockEnv = MockEnv
  { loanEnv :: Env.LoanEnv,
    tokenEnv :: Env.TokenEnv
  }
  deriving (CakeSlayer.Has Env.LoanEnv) via CakeSlayer.Field "loanEnv" MockEnv
  deriving (CakeSlayer.Has Env.TokenEnv) via CakeSlayer.Field "tokenEnv" MockEnv

mockEnv :: MockEnv
mockEnv =
  MockEnv
    { loanEnv =
        Env.LoanEnv
          { bSelectionUser = undefined,
            bSelectionItem = undefined,
            bSelectionLoan = undefined,
            bDatabaseLoan = pure (Db.create (Loan.loan "1") Db.empty),
            bFilterUser = undefined,
            bFilterItem = undefined,
            bFilterLoan = pure "",
            bModalState = undefined
          },
      tokenEnv =
        Env.TokenEnv
          { bDatabaseToken = pure Db.empty,
            bSelectionToken = pure Nothing,
            bTTL = pure (Just (Time.secondsToNominalDiffTime 100))
          }
    }

type MockApp = CakeSlayer.App Void MockEnv

instance Write.MonadWrite MockApp (Db.Database Loan.Loan) where
  write = Write.writeImpl

instance Read.MonadRead MockApp (Db.Database Loan.Loan) where
  read = Read.readImpl

instance Time.MonadTime MockApp where
  currentTime = Time.currentTimeImpl

instance Time.MonadParseTime MockApp where
  parseTime = Time.parseTimeImpl

runMockApp :: MockApp a -> IO a
runMockApp = CakeSlayer.runApp mockEnv

mockConfig :: Config.Config
mockConfig =
  Config.Config
    { datastoreLoan = "./test/data/loan.json",
      datastoreUser = "./test/data/user.json",
      datastoreRole = "./test/data/role.json",
      datastorePrivilege = "./test/data/privilege.json",
      datastoreToken = "./test/data/token.json",
      datastoreItem = "./test/data/item.json",
      datastoreTab = "./test/data/tab.json"
    }

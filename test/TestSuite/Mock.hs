{-# LANGUAGE DataKinds #-}

module TestSuite.Mock
  ( runMockApp,
    mockConfig,
  )
where

import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Write as Write

data MockEnv = MockEnv
  { loanEnv :: Env.LoanEnv
  }
  deriving (CakeSlayer.Has Env.LoanEnv) via CakeSlayer.Field "loanEnv" MockEnv

mockEnv :: MockEnv
mockEnv =
  MockEnv
    { loanEnv =
        Env.LoanEnv
          { bDatabaseLoan = undefined,
            bSelectionUser = undefined,
            bSelectionItem = undefined,
            bSelectionLoan = undefined,
            bFilterUser = undefined,
            bFilterItem = undefined,
            bFilterLoan = undefined,
            bModalState = undefined
          }
    }

type MockApp = CakeSlayer.App Void MockEnv

instance Write.MonadWrite MockApp (Db.Database Loan.Loan) where
  write = Write.writeImpl

instance Read.MonadRead MockApp (Db.Database Loan.Loan) where
  read = Read.readImpl

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

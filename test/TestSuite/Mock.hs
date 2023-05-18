{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module TestSuite.Mock
  ( runMockApp,
    MockEnv (..),
    MockApp,
    mockConfig,
    mockEnv,
  )
where

import Data.Aeson (FromJSON, decode, throwDecodeStrict)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Graphics.UI.Threepenny.Core as UI
import Piece
import Piece.App.Env (PrivilegeEnv (bDatabasePrivilege))
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer as CakeSlayer
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import Piece.Core.Privilege (Privilege)
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Write as Write
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

data MockEnv = MockEnv
  { loanEnv :: Env.LoanEnv,
    tokenEnv :: Env.TokenEnv,
    timeEnv :: Env.TimeEnv,
    roleEnv :: Env.RoleEnv,
    privilegeEnv :: Env.PrivilegeEnv,
    userEnv :: UserEnv.UserEnv,
    hToken :: R.Handler (Maybe Token.Token), -- should reflect app events
    hUser :: R.Handler (Maybe User.User), -- should reflect app events
    hRole :: R.Handler (Role.Role), -- should reflect app events this will never
    hPrivilege :: R.Handler (Privilege.Privilege), -- should reflect app events this will never
    hTime :: R.Handler Time.Time -- should reflect app events this will never
  }
  deriving (CakeSlayer.Has Env.LoanEnv) via CakeSlayer.Field "loanEnv" MockEnv
  deriving (CakeSlayer.Has Env.TokenEnv) via CakeSlayer.Field "tokenEnv" MockEnv
  deriving (CakeSlayer.Has Env.TimeEnv) via CakeSlayer.Field "timeEnv" MockEnv
  deriving (CakeSlayer.Has UserEnv.UserEnv) via CakeSlayer.Field "userEnv" MockEnv
  deriving (CakeSlayer.Has Env.RoleEnv) via CakeSlayer.Field "roleEnv" MockEnv
  deriving (CakeSlayer.Has (R.Handler (Maybe Token.Token))) via CakeSlayer.Field "hToken" MockEnv
  deriving (CakeSlayer.Has (R.Handler (Maybe User.User))) via CakeSlayer.Field "hUser" MockEnv
  deriving (CakeSlayer.Has (R.Handler (Role.Role))) via CakeSlayer.Field "hRole" MockEnv
  deriving (CakeSlayer.Has (R.Handler (Privilege.Privilege))) via CakeSlayer.Field "hPrivilege" MockEnv
  deriving (CakeSlayer.Has (R.Handler (Time.Time))) via CakeSlayer.Field "hTime" MockEnv

mockEnv :: MockApp MockEnv
mockEnv = mdo
  let config = mockConfig
  (eToken, hToken) <- liftIO $ R.newEvent
  bSelectionToken <- R.stepper (Just 0) $ Unsafe.head <$> R.unions []
  bTTL <- R.stepper (Just (Time.secondsToNominalDiffTime 100)) $ Unsafe.head <$> R.unions []
  bDatabaseToken <- R.stepper Db.empty $ Unsafe.head <$> R.unions [flip . Db.update . Unsafe.fromJust <$> bSelectionToken <*> bDatabaseToken UI.<@> UI.filterJust eToken]

  -- Time
  (eTime, hTime) <- liftIO $ R.newEvent
  timeEnv <- timeEnvSetup config eTime

  -- User
  (eUser, hUser) <- liftIO $ R.newEvent
  userEnv <- userEnvSetup config R.never eUser R.never R.never R.never

  -- Role
  (eRole, hRole) <- liftIO $ R.newEvent
  roleEnv <- roleEnvSetup config (flip Db.create <$> (Env.bDatabaseRole roleEnv) UI.<@> eRole)

  -- Privilege
  (ePrivilege, hPrivilege) <- liftIO $ R.newEvent
  privilegeEnv <- privilegeEnvSetup config (flip Db.create <$> (Env.bDatabasePrivilege privilegeEnv) UI.<@> ePrivilege)

  return $
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
        timeEnv = timeEnv,
        tokenEnv =
          Env.TokenEnv
            { bDatabaseToken = bDatabaseToken,
              bSelectionToken = bSelectionToken,
              bTTL = bTTL
            },
        userEnv = userEnv,
        roleEnv = roleEnv,
        privilegeEnv = privilegeEnv,
        hToken = hToken,
        hRole = hRole,
        hPrivilege = hPrivilege,
        hUser = hUser,
        hTime = hTime
      }

type MockApp = CakeSlayer.App Void MockEnv

instance Write.MonadWrite MockApp (Db.Database Loan.Loan) where
  write = Write.writeImpl

instance FromJSON a => Read.MonadRead MockApp a where
  read = Read.readImpl

instance Time.MonadTime MockApp where
  currentTime = Time.currentTimeImpl

instance Time.MonadParseTime MockApp where
  parseTime = Time.parseTimeImpl

runMockApp :: MockApp a -> IO a
runMockApp app = mdo
  env <- CakeSlayer.runApp env mockEnv
  CakeSlayer.runApp env app

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

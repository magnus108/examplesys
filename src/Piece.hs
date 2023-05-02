{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import qualified Control.Monad.Fix as Fix
import qualified Control.Monad.IO.Unlift as UnliftIO
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import Piece.App.Monad (runApp)
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Tab as Tab
import qualified Piece.Core.Time as Time (time, unTime)
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Write as Write
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Piece.Gui.Tab.Tab as Tab
import qualified Piece.Gui.Time.Time as GuiTime
import qualified Piece.Gui.User.Create as UserCreate
import qualified Piece.Gui.User.Login as UserLogin
import qualified Piece.Time.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

main :: Int -> IO ()
main port = do
  config <- Config.load
  UI.startGUI
    UI.defaultConfig
      { UI.jsPort = Just port,
        UI.jsStatic = Just "./static",
        UI.jsCustomHTML = Just "index.html",
        UI.jsWindowReloadOnDisconnect = False,
        UI.jsCallBufferMode = UI.NoBuffering
      }
    $ \window -> mdo
      -- READ
      databaseLoan <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastoreLoan config)
      databaseTab <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastoreTab config)
      databaseRole <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastoreRole config)
      databaseUser <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastoreUser config)
      databasePrivilege <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastorePrivilege config)
      databaseToken <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastoreToken config)
      -- TIMER
      time <- liftIO $ Monad.runApp env $ Error.tryError Time.currentTime
      eTime <- Time.timer env

      -- GUI
      loanCreate <- LoanCreate.setup env
      userCreate <- UserCreate.setup env
      userLogin <- UserLogin.setup env

      tabs <- Tab.setup env
      _ <- UI.getBody window UI.#+ [UI.element tabs]

      -- LISTEN
      _ <- UI.liftIOLater $ Monad.runApp env $ listen config

      -- BEHAVIOR
      let tSelectionTab = Tab.tTabSelection tabs
          eSelectionTab = UI.rumors tSelectionTab

      bTTL <- R.stepper (Time.secondsToNominalDiffTime 100) $ Unsafe.head <$> R.unions []

      bTime <- R.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTime]

      bDatabaseTab <- R.stepper (fromRight Db.empty databaseTab) $ Unsafe.head <$> R.unions []
      bSelectionTab <- R.stepper (Just 0) $ Unsafe.head <$> R.unions [eSelectionTab]
      bViewMapTab <-
        R.stepper
          ( Map.fromList
              [ (0, UI.element loanCreate),
                (1, UI.element userCreate),
                (2, UI.element userLogin)
              ]
          )
          $ Unsafe.head <$> R.unions []

      validate <- liftIO $ Monad.runApp env $ Token.validate
      let eToken = validate UI.<@> eTime
          (eInvalidToken, eValidToken) = UI.split eToken
      bSelectionToken <- R.stepper Nothing $ Unsafe.head <$> R.unions [fmap Just eValidToken, Nothing <$ eInvalidToken]
      bDatabaseToken <- R.stepper (fromRight Db.empty databaseToken) $ Unsafe.head <$> R.unions []

      userEnv <- userEnvSetup userCreate userLogin databaseUser
      loanEnv <- loanEnvSetup loanCreate databaseLoan
      roleEnv <- roleEnvSetup databaseRole
      privilegeEnv <- privilegeEnvSetup databasePrivilege

      -- ENV
      let env =
            Env.Env
              { tabEnv =
                  Env.TabEnv
                    { bDatabaseTab = bDatabaseTab,
                      bSelectionTab = bSelectionTab,
                      bViewMapTab = bViewMapTab
                    },
                timeEnv =
                  Env.TimeEnv
                    { bTime = bTime
                    },
                loanEnv = loanEnv,
                roleEnv = roleEnv,
                userEnv = userEnv,
                privilegeEnv = privilegeEnv,
                tokenEnv =
                  Env.TokenEnv
                    { bDatabaseToken = bDatabaseToken,
                      bSelectionToken = bSelectionToken,
                      bTTL = bTTL
                    }
              }

      -- RETURN
      return ()

listen ::
  ( MonadReader env m,
    Has.Has Env.LoanEnv env,
    Has.Has UserEnv.UserEnv env,
    Has.Has Env.TabEnv env,
    Write.MonadWrite m (Db.Database Tab.Tab),
    Write.MonadWrite m (Db.Database Loan.Loan),
    Write.MonadWrite m (Db.Database User.User),
    UnliftIO.MonadUnliftIO m
  ) =>
  Config.Config ->
  m ()
listen config = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  UnliftIO.withRunInIO $ \run -> do
    _ <- UI.liftIO $ R.onChange bDatabaseLoan $ \s -> run $ Write.write (Config.datastoreLoan config) s
    _ <- UI.liftIO $ R.onChange bDatabaseTab $ \s -> run $ Write.write (Config.datastoreTab config) s
    _ <- UI.liftIO $ R.onChange bDatabaseUser $ \s -> run $ Write.write (Config.datastoreUser config) s
    return ()

userCreateSetup :: MonadIO m => UserCreate.Create -> m (R.Behavior (Maybe User.User))
userCreateSetup userCreate = do
  let tUserCreate = UserCreate.tUserCreate userCreate
      eUserCreate = UI.rumors tUserCreate
  R.stepper Nothing $ Unsafe.head <$> R.unions [eUserCreate]

userLoginSetup :: MonadIO m => UserLogin.Create -> m (R.Behavior (Maybe Db.DatabaseKey))
userLoginSetup userLogin = do
  let tUserLogin = UserLogin.tUserLogin userLogin
      eUserLogin = UI.rumors tUserLogin
  R.stepper Nothing $ Unsafe.head <$> R.unions [eUserLogin]

userCreateFormSetup :: MonadIO m => UserCreate.Create -> m (R.Behavior (Maybe UserCreateForm.User))
userCreateFormSetup userCreate = do
  let tUserCreateForm = UserCreate.tUserCreateForm userCreate
      eUserCreateForm = UI.rumors tUserCreateForm

  let tUserCreate = UserCreate.tUserCreate userCreate
      eUserCreate = UI.rumors tUserCreate

  R.stepper Nothing $ Unsafe.head <$> R.unions [Just <$> eUserCreateForm, Nothing <$ eUserCreate]

userLoginFormSetup :: MonadIO m => UserLogin.Create -> m (R.Behavior (Maybe UserLoginForm.User))
userLoginFormSetup userLogin = do
  let tUserLoginForm = UserLogin.tUserLoginForm userLogin
      eUserLoginForm = UI.rumors tUserLoginForm

  let tUserLogin = UserLogin.tUserLogin userLogin
      eUserLogin = UI.rumors tUserLogin

  R.stepper Nothing $ Unsafe.head <$> R.unions [Just <$> eUserLoginForm, Nothing <$ eUserLogin]

databaseUserSetup :: (Fix.MonadFix m, MonadIO m) => Either e (Db.Database User.User) -> UserCreate.Create -> m (R.Behavior (Db.Database User.User))
databaseUserSetup databaseUser userCreate = mdo
  let tUserCreate = UserCreate.tUserCreate userCreate
      eUserCreate = UI.rumors tUserCreate
  bDatabaseUser <- R.stepper (fromRight Db.empty databaseUser) $ Unsafe.head <$> R.unions [flip Db.create <$> bDatabaseUser UI.<@> (R.filterJust eUserCreate)]
  return bDatabaseUser

userEnvSetup :: (MonadIO m, Fix.MonadFix m) => UserCreate.Create -> UserLogin.Create -> Either e (Db.Database User.User) -> m UserEnv.UserEnv
userEnvSetup userCreate userLogin databaseUser = do
  bUserCreateForm <- userCreateFormSetup userCreate
  bUserCreate <- userCreateSetup userCreate
  bUserLoginForm <- userLoginFormSetup userLogin
  bUserLogin <- userLoginSetup userLogin
  bDatabaseUser <- databaseUserSetup databaseUser userCreate
  return $
    UserEnv.UserEnv
      { bDatabaseUser = bDatabaseUser,
        bUserCreate = bUserCreate,
        bUserCreateForm = bUserCreateForm,
        bUserLoginForm = bUserLoginForm,
        bUserLogin = bUserLogin
      }

loanEnvSetup :: (MonadIO m, Fix.MonadFix m) => LoanCreate.Create -> Either e (Db.Database Loan.Loan) -> m Env.LoanEnv
loanEnvSetup loanCreate databaseLoan = do
  let tLoanFilter = LoanCreate.tLoanFilter loanCreate
  let eLoanFilter = R.rumors tLoanFilter

  bDatabaseLoan <- R.stepper (fromRight Db.empty databaseLoan) $ Unsafe.head <$> R.unions []
  bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter]
  bModalState <- R.stepper False $ Unsafe.head <$> R.unions []

  return $
    Env.LoanEnv
      { bDatabaseLoan = bDatabaseLoan,
        bSelectionUser = bSelectionUser,
        bSelectionItem = bSelectionItem,
        bSelectionLoan = bSelectionLoan,
        bFilterUser = bFilterUser,
        bFilterItem = bFilterItem,
        bFilterLoan = bFilterLoan,
        bModalState = bModalState
      }

roleEnvSetup :: (MonadIO m, Fix.MonadFix m) => Either e (Db.Database Role.Role) -> m Env.RoleEnv
roleEnvSetup databaseRole = do
  bDatabaseRole <- R.stepper (fromRight Db.empty databaseRole) $ Unsafe.head <$> R.unions []
  return $ Env.RoleEnv {bDatabaseRole = bDatabaseRole}

privilegeEnvSetup :: (MonadIO m, Fix.MonadFix m) => Either e (Db.Database Privilege.Privilege) -> m Env.PrivilegeEnv
privilegeEnvSetup databasePrivilege = do
  bDatabasePrivilege <- R.stepper (fromRight Db.empty databasePrivilege) $ Unsafe.head <$> R.unions []
  return $ Env.PrivilegeEnv {bDatabasePrivilege = bDatabasePrivilege}

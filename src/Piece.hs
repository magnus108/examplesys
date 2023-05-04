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
import qualified Data.Time.LocalTime as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Timer as UI
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
import qualified Piece.Core.Time as Time (Time, time, unTime)
import qualified Piece.Core.Token as Token
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
import qualified Piece.Gui.Tab.Tab2 as Tab2
import qualified Piece.Gui.Time.Time as GuiTime
import qualified Piece.Gui.User.Create as UserCreate
import qualified Piece.Gui.User.Login as UserLogin
import qualified Piece.Time.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import System.Random

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
    $ \window ->
      void $ do
        ((i, btn), sa) <- liftIO $
          mdo
            (env, ui, sa) <- Monad.runApp env $ mdo
              -- databaseTab <- Error.tryError $ Read.read (Config.datastoreTab config)
              traceShowM "a"
              time <- Error.tryError Time.currentTime
              traceShowM "a"
              (te, th) <- liftIO $ R.newEvent
              traceShowM "a"
              traceShowM "a"
              t <- liftIO $ UI.runUI window $ UI.timer UI.# UI.set UI.interval 1000 UI.# UI.set UI.running True

              traceShowM "a"
              btn <- liftIO $ UI.runUI window $ UI.button UI.#+ [UI.string "bob"]
              bTime <- liftIO $ UI.runUI window $ R.stepper (0 :: Int) $ Unsafe.head <$> R.unions [te]

              traceShowM "a"
              i <- liftIO $ UI.runUI window $ UI.div UI.# UI.sink UI.text (show <$> bTime)

              liftIO $ UI.runUI window $ UI.getBody window UI.#+ [UI.element i, UI.element btn]
              traceShowM "b"

              traceShowM "c"

              sa <- liftIO $ UI.runUI window $ UI.liftIOLater $ do
                x <- R.register (UI.tick t) $ \_ -> do
                  time <- randomIO :: IO Int
                  traceShowM time
                  th time
                return ()

              traceShowM "x"
              let env =
                    Env.Env
                      { window = window,
                        tabEnv = undefined,
                        timeEnv = undefined,
                        loanEnv = undefined,
                        roleEnv = undefined,
                        userEnv = undefined,
                        privilegeEnv = undefined
                      }
              traceShowM "d"
              return (env, (btn, i), sa)
            return (ui, sa)
        traceShowM "mand"
        traceShowM "ddada"

{-
      -- READ
      databaseTab <- liftIO $ Monad.runApp env $ Error.tryError $ Read.read (Config.datastoreTab config)
      -- TIMER
      time <- liftIO $ Monad.runApp env $ Error.tryError Time.currentTime
      eTime <- Time.timer env

      -- GUI
      loanCreate <- LoanCreate.setup env
      userCreate <- UserCreate.setup env
      userLogin <- UserLogin.setup env

      tabs <- Tab.setup env
      tabs2 <- Tab2.setup env
      _ <- UI.getBody window UI.#+ [UI.element tabs, UI.element tabs2]

      -- LISTEN
      _ <- UI.liftIOLater $ Monad.runApp env $ listen config

      -- BEHAVIOR

      ---------TAB
      let tSelectionTab = Tab.tTabSelection tabs
          eSelectionTab = UI.rumors tSelectionTab

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

      ---------TOKEN
      bTime <- R.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTime]
      {-
      validate <- liftIO $ Monad.runApp env $ Token.validate
      let eToken = validate UI.<@> eTime
          (eInvalidToken, eValidToken) = UI.split eToken
      bSelectionToken <- R.stepper Nothing $ Unsafe.head <$> R.unions [fmap Just eValidToken, Nothing <$ eInvalidToken]
      -}

      -- ENV hvorfor får jeg ik fejl?
      userEnv <- liftIO $ Monad.runApp env $ userEnvSetup config userCreate userLogin
      loanEnv <- liftIO $ Monad.runApp env $ loanEnvSetup config loanCreate
      roleEnv <- liftIO $ Monad.runApp env $ roleEnvSetup config
      privilegeEnv <- liftIO $ Monad.runApp env $ privilegeEnvSetup config

let env =
      Env.Env
        { window = window,
          tabEnv =
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
          privilegeEnv = privilegeEnv
        }

-- RETURN
return ()
-}

listen ::
  ( MonadReader env m,
    Has.Has Env.LoanEnv env,
    Has.Has UserEnv.UserEnv env,
    Has.Has Env.TabEnv env,
    Write.MonadWrite m (Db.Database Token.Token),
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
  let bDatabaseToken = UserEnv.bDatabaseToken userEnv
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  UnliftIO.withRunInIO $ \run -> do
    _ <- UI.liftIO $ R.onChange bDatabaseLoan $ \s -> run $ Write.write (Config.datastoreLoan config) s
    _ <- UI.liftIO $ R.onChange bDatabaseTab $ \s -> run $ Write.write (Config.datastoreTab config) s
    _ <- UI.liftIO $ R.onChange bDatabaseUser $ \s -> run $ Write.write (Config.datastoreUser config) s
    _ <- UI.liftIO $ R.onChange bDatabaseToken $ \s -> run $ Write.write (Config.datastoreToken config) s
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

databaseUserSetup :: (Fix.MonadFix m, MonadIO m, Read.MonadRead m (Db.Database User.User)) => Config.Config -> UserCreate.Create -> m (R.Behavior (Db.Database User.User))
databaseUserSetup config userCreate = mdo
  databaseUser <- Read.read (Config.datastoreUser config)
  let tUserCreate = UserCreate.tUserCreate userCreate
      eUserCreate = UI.rumors tUserCreate
  bDatabaseUser <- R.stepper databaseUser $ Unsafe.head <$> R.unions [flip Db.create <$> bDatabaseUser UI.<@> (R.filterJust eUserCreate)]
  return bDatabaseUser

databaseTokenSetup :: (Env.WithTimeEnv env m, Env.WithUserEnv env m, Fix.MonadFix m, MonadIO m, Read.MonadRead m (Db.Database Token.Token)) => Config.Config -> UserLogin.Create -> m (R.Behavior (Db.Database Token.Token))
databaseTokenSetup config userLogin = mdo
  databaseToken <- Read.read (Config.datastoreToken config)

  let tUserLogin = UserLogin.tUserLogin userLogin
      eUserLogin = UI.rumors tUserLogin

  userEnv <- Has.grab @UserEnv.UserEnv
  let bSelectionToken = UserEnv.bSelectionToken userEnv

  timeEnv <- Has.grab @Env.TimeEnv
  let bTime = Env.bTime timeEnv

  let eToken = fmap . flip Token.token <$> bTime UI.<@> eUserLogin

  bDatabaseToken <-
    R.stepper databaseToken $
      Unsafe.head
        <$> R.unions [flip . Db.update . Unsafe.fromJust <$> bSelectionToken <*> bDatabaseToken UI.<@> UI.filterJust eToken]

  return bDatabaseToken

selectionTokenSetup :: (Fix.MonadFix m, MonadIO m) => m (R.Behavior (Maybe Db.DatabaseKey))
selectionTokenSetup = do
  R.stepper (Just 0) $ Unsafe.head <$> R.unions []

ttlSetup :: (Fix.MonadFix m, MonadIO m) => m (R.Behavior (Maybe Time.NominalDiffTime))
ttlSetup = R.stepper (Just (Time.secondsToNominalDiffTime 100)) $ Unsafe.head <$> R.unions []

userEnvSetup :: (Env.WithUserEnv env m, Env.WithTimeEnv env m, MonadIO m, Fix.MonadFix m, Read.MonadRead m (Db.Database Token.Token), Read.MonadRead m (Db.Database User.User)) => Config.Config -> UserCreate.Create -> UserLogin.Create -> m UserEnv.UserEnv
userEnvSetup config userCreate userLogin = do
  bUserCreateForm <- userCreateFormSetup userCreate
  bUserCreate <- userCreateSetup userCreate
  bUserLoginForm <- userLoginFormSetup userLogin
  bUserLogin <- userLoginSetup userLogin
  bDatabaseUser <- databaseUserSetup config userCreate
  bDatabaseToken <- databaseTokenSetup config userLogin
  bTTL <- ttlSetup
  bSelectionToken <- selectionTokenSetup
  return $
    UserEnv.UserEnv
      { bDatabaseUser = bDatabaseUser,
        bUserCreate = bUserCreate,
        bUserCreateForm = bUserCreateForm,
        bUserLoginForm = bUserLoginForm,
        bUserLogin = bUserLogin,
        bDatabaseToken = bDatabaseToken,
        bTTL = bTTL,
        bSelectionToken = bSelectionToken
      }

loanEnvSetup :: (MonadIO m, Fix.MonadFix m, Read.MonadRead m (Db.Database Loan.Loan)) => Config.Config -> LoanCreate.Create -> m Env.LoanEnv
loanEnvSetup config loanCreate = do
  databaseLoan <- Read.read (Config.datastoreLoan config)
  let tLoanFilter = LoanCreate.tLoanFilter loanCreate
  let eLoanFilter = R.rumors tLoanFilter

  bDatabaseLoan <- R.stepper databaseLoan $ Unsafe.head <$> R.unions []
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

roleEnvSetup :: (MonadIO m, Fix.MonadFix m, Read.MonadRead m (Db.Database Role.Role)) => Config.Config -> m Env.RoleEnv
roleEnvSetup config = do
  databaseRole <- Read.read (Config.datastoreRole config)
  bDatabaseRole <- R.stepper databaseRole $ Unsafe.head <$> R.unions []
  return $ Env.RoleEnv {bDatabaseRole = bDatabaseRole}

privilegeEnvSetup ::
  ( MonadIO m,
    Fix.MonadFix m,
    Read.MonadRead m (Db.Database Privilege.Privilege)
  ) =>
  Config.Config ->
  m Env.PrivilegeEnv
privilegeEnvSetup config = do
  databasePrivilege <- Read.read (Config.datastorePrivilege config)
  bDatabasePrivilege <- R.stepper databasePrivilege $ Unsafe.head <$> R.unions []
  return $ Env.PrivilegeEnv {bDatabasePrivilege = bDatabasePrivilege}

{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
    roleEnvSetup,
    tokenEnvSetup,
    timeEnvSetup,
    userEnvSetup,
    privilegeEnvSetup,
    tabEnvSetup,
  )
where

import qualified Control.Monad.Fix as Fix
import qualified Control.Monad.IO.Unlift as UnliftIO
import qualified Data.Time.Clock as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Tab as Tab
import qualified Piece.Core.Time as Time
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
import qualified Piece.Gui.Tab.TabButton as TabButton
import qualified Piece.Gui.Tab.TabView as TabView
import qualified Piece.Gui.User.Create as UserCreate
import qualified Piece.Gui.User.Edit as UserEdit
import qualified Piece.Gui.User.List as UserList
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
      -- GUI
      loanCreate <- LoanCreate.setup env
      userCreate <- UserCreate.setup env
      userLogin <- UserLogin.setup env
      userList <- UserList.setup env
      userEdit <- UserEdit.setup env

      let tabViews =
            [ UI.getElement loanCreate,
              UI.getElement userCreate,
              UI.getElement userLogin,
              UI.getElement userList,
              UI.getElement userEdit
            ]

      tabs <- TabButton.setup env
      views <- TabView.setup env tabViews
      _ <- UI.getBody window UI.#+ [UI.element tabs, UI.element views]

      -- LISTEN
      _ <- UI.liftIOLater $ Monad.runApp env $ listen config

      -- BEHAVIOR

      env <- liftIO $ Monad.runApp env $ do
        eTime <- Time.timer window

        let tUserCreate = UserCreate.tUserCreate userCreate
            eUserCreate = UI.rumors tUserCreate

        let tUserCreateForm = UserCreate.tUserCreateForm userCreate
            eUserCreateForm = UI.rumors tUserCreateForm

        let tUserLoginForm = UserLogin.tUserLoginForm userLogin
            eUserLoginForm = UI.rumors tUserLoginForm

        let tUserLogin = UserLogin.tUserLogin userLogin
            eUserLogin = UI.rumors tUserLogin

        let tSelectionTab = TabButton.userSelection tabs
            eSelectionTab = UI.rumors tSelectionTab

        let tUserFilter = UserList.tUserFilter userList
            eUserFilter = R.rumors tUserFilter

        let tUserSelection = UserList.tUserSelection userList
            eUserSelect = R.rumors tUserSelection

        let tUserDelete = UserList.tUserDelete userList
            eUserDelete = R.rumors tUserDelete

        ---- EDIT
        let tUserFilterEdit = UserEdit.tUserFilter userEdit
            eUserFilterEdit = R.rumors tUserFilterEdit

        let tUserSelectionEdit = UserEdit.tUserSelection userEdit
            eUserSelectionEdit = R.rumors tUserSelectionEdit

        let tUserEdit = UserEdit.tUserEdit userEdit
            eUserEdit = R.rumors tUserEdit

        timeEnv <- timeEnvSetup config eTime
        userEnv <- userEnvSetup config eUserCreateForm eUserCreate eUserLoginForm eUserLogin eTime eUserSelect eUserFilter eUserDelete {-edit-} eUserSelectionEdit eUserFilterEdit eUserEdit
        tokenEnv <- tokenEnvSetup config eUserLogin eTime
        loanEnv <- loanEnvSetup config loanCreate
        roleEnv <- roleEnvSetup config R.never
        privilegeEnv <- privilegeEnvSetup config R.never
        tabEnv <- tabEnvSetup config R.never eSelectionTab

        let env =
              Env.Env
                { tabEnv = tabEnv,
                  timeEnv = timeEnv,
                  loanEnv = loanEnv,
                  roleEnv = roleEnv,
                  userEnv = userEnv,
                  tokenEnv = tokenEnv,
                  privilegeEnv = privilegeEnv
                }

        return env

      -- RETURN
      return ()

listen ::
  ( MonadReader env m,
    Has.Has Env.LoanEnv env,
    Has.Has UserEnv.UserEnv env,
    Has.Has Env.TabEnv env,
    Has.Has Env.TokenEnv env,
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
  tokenEnv <- Has.grab @Env.TokenEnv
  let bDatabaseUser = UserEnv.bDatabaseUser userEnv
  let bDatabaseToken = Env.bDatabaseToken tokenEnv
  tabEnv <- Has.grab @Env.TabEnv
  let bDatabaseTab = Env.bDatabaseTab tabEnv
  loanEnv <- Has.grab @Env.LoanEnv
  let bDatabaseLoan = Env.bDatabaseLoan loanEnv
  UnliftIO.withRunInIO $ \run -> do
    _ <- UI.liftIO $ R.onChange bDatabaseLoan $ \s -> run $ void $ Write.write (Config.datastoreLoan config) s
    _ <- UI.liftIO $ R.onChange bDatabaseTab $ \s -> run $ void $ Write.write (Config.datastoreTab config) s
    _ <- UI.liftIO $ R.onChange bDatabaseUser $ \s -> run $ void $ Write.write (Config.datastoreUser config) s
    _ <- UI.liftIO $ R.onChange bDatabaseToken $ \s -> run $ void $ Write.write (Config.datastoreToken config) s
    return ()

timeEnvSetup :: (Time.MonadTime m, MonadIO m) => Config.Config -> R.Event Time.Time -> m (Env.TimeEnv)
timeEnvSetup config eTime = do
  time <- Time.currentTime
  bTime <- R.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTime]
  return $
    Env.TimeEnv
      { Env.bTime = bTime
      }

tabEnvSetup :: (Read.MonadRead m (Db.Database Tab.Tab), MonadIO m) => Config.Config -> R.Event (Db.Database Tab.Tab) -> R.Event (Maybe Db.DatabaseKey) -> m (Env.TabEnv)
tabEnvSetup config eTab eSelectionTab = do
  databaseTab <- Read.read (Config.datastoreTab config)
  bDatabaseTab <- R.stepper (fromRight Db.empty databaseTab) $ Unsafe.head <$> R.unions [eTab]
  bSelectionTab <- R.stepper (Just 0) $ Unsafe.head <$> R.unions [eSelectionTab]
  return $
    Env.TabEnv
      { bDatabaseTab = bDatabaseTab,
        bSelectionTab = bSelectionTab
      }

userCreateSetup :: MonadIO m => R.Event (Maybe User.User) -> m (R.Behavior (Maybe User.User))
userCreateSetup eUserCreate = do
  R.stepper Nothing $ Unsafe.head <$> R.unions [eUserCreate]

userLoginSetup :: MonadIO m => R.Event (Maybe Db.DatabaseKey) -> m (R.Behavior (Maybe Db.DatabaseKey))
userLoginSetup eUserLogin = do
  R.stepper Nothing $ Unsafe.head <$> R.unions [eUserLogin]

userCreateFormSetup :: MonadIO m => R.Event UserCreateForm.User -> R.Event (Maybe User.User) -> m (R.Behavior (Maybe UserCreateForm.User))
userCreateFormSetup eUserCreateForm eUserCreate = do
  R.stepper Nothing $ Unsafe.head <$> R.unions [Just <$> eUserCreateForm, Nothing <$ eUserCreate]

userLoginFormSetup :: MonadIO m => R.Event UserLoginForm.User -> R.Event (Maybe Db.DatabaseKey) -> m (R.Behavior (Maybe UserLoginForm.User))
userLoginFormSetup eUserLoginForm eUserLogin = do
  R.stepper Nothing $ Unsafe.head <$> R.unions [Just <$> eUserLoginForm, Nothing <$ eUserLogin]

databaseUserSetup :: (Fix.MonadFix m, MonadIO m, Read.MonadRead m (Db.Database User.User)) => Config.Config -> R.Event User.User -> R.Event (Maybe Db.DatabaseKey) -> m (R.Behavior (Db.Database User.User))
databaseUserSetup config eUserCreate eUserDelete = mdo
  databaseUser <- Read.read (Config.datastoreUser config)
  bDatabaseUser <-
    R.stepper (fromRight Db.empty databaseUser) $
      Unsafe.head
        <$> R.unions
          [ flip Db.create <$> bDatabaseUser UI.<@> eUserCreate,
            flip Db.delete <$> bDatabaseUser UI.<@> (R.filterJust eUserDelete)
          ]

  return bDatabaseUser

databaseTokenSetup ::
  ( Env.WithTokenEnv env m,
    Env.WithTimeEnv env m,
    Fix.MonadFix m,
    MonadIO m,
    Read.MonadRead m (Db.Database Token.Token)
  ) =>
  Config.Config ->
  R.Event (Maybe Db.DatabaseKey) ->
  R.Event Time.Time ->
  m (R.Behavior (Db.Database Token.Token))
databaseTokenSetup config eUserLogin eTime = mdo
  databaseToken <- Read.read (Config.datastoreToken config)

  tokenEnv <- Has.grab @Env.TokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv

  timeEnv <- Has.grab @Env.TimeEnv
  let bTime = Env.bTime timeEnv

  let eToken = fmap . flip Token.token <$> bTime UI.<@> eUserLogin

  bValidate <- Token.validate
  let eValidateToken = bValidate UI.<@> eTime

  bDatabaseToken <-
    R.stepper (fromRight Db.empty databaseToken) $
      Unsafe.head
        <$> R.unions
          [ flip . Db.update . Unsafe.fromJust <$> bSelectionToken <*> bDatabaseToken UI.<@> UI.filterJust eToken,
            Db.delete . Unsafe.fromJust <$> bSelectionToken <*> bDatabaseToken UI.<@ R.filterE isNothing eValidateToken
          ]

  return bDatabaseToken

selectionTokenSetup :: (MonadIO m) => m (R.Behavior (Maybe Db.DatabaseKey))
selectionTokenSetup = do
  R.stepper (Just 0) $ Unsafe.head <$> R.unions []

ttlSetup :: (MonadIO m) => m (R.Behavior (Maybe Time.NominalDiffTime))
ttlSetup = R.stepper (Just (Time.secondsToNominalDiffTime 100)) $ Unsafe.head <$> R.unions []

tokenEnvSetup ::
  ( Env.WithTokenEnv env m,
    Env.WithTimeEnv env m,
    MonadIO m,
    Fix.MonadFix m,
    Read.MonadRead m (Db.Database Token.Token)
  ) =>
  Config.Config ->
  R.Event (Maybe Db.DatabaseKey) ->
  R.Event Time.Time ->
  m Env.TokenEnv
tokenEnvSetup config eUserLogin eTime = do
  bDatabaseToken <- databaseTokenSetup config eUserLogin eTime
  bTTL <- ttlSetup
  bSelectionToken <- selectionTokenSetup
  return $
    Env.TokenEnv
      { bDatabaseToken = bDatabaseToken,
        bTTL = bTTL,
        bSelectionToken = bSelectionToken
      }

userEnvSetup ::
  (Env.WithUserEnv env m, MonadIO m, Fix.MonadFix m, Read.MonadRead m (Db.Database User.User)) =>
  Config.Config ->
  R.Event UserCreateForm.User ->
  R.Event (Maybe User.User) ->
  R.Event UserLoginForm.User ->
  R.Event (Maybe Db.DatabaseKey) ->
  R.Event Time.Time ->
  R.Event (Maybe Db.DatabaseKey) ->
  R.Event String ->
  R.Event (Maybe Db.DatabaseKey) ->
  -- EDIT
  R.Event (Maybe Db.DatabaseKey) ->
  R.Event String ->
  R.Event (Maybe Db.DatabaseKey) ->
  m UserEnv.UserEnv
userEnvSetup config eUserCreateForm eUserCreate eUserLoginForm eUserLogin eTime eSelectUser eFilterUser eUserDelete eSelectUserEdit eFilterUserEdit eUserEdit = do
  bUserCreateForm <- userCreateFormSetup eUserCreateForm eUserCreate
  bUserCreate <- userCreateSetup eUserCreate
  bUserLoginForm <- userLoginFormSetup eUserLoginForm eUserLogin
  bUserLogin <- userLoginSetup eUserLogin
  bDatabaseUser <- databaseUserSetup config (R.filterJust eUserCreate) eUserDelete

  bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions [eSelectUser, Nothing <$ eUserDelete]
  bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions [eFilterUser]
  bUserDelete <- R.stepper Nothing $ Unsafe.head <$> R.unions [eUserDelete]

  bSelectionUserEdit <- R.stepper Nothing $ Unsafe.head <$> R.unions [eSelectUserEdit, Nothing <$ eUserEdit]
  bFilterUserEdit <- R.stepper "" $ Unsafe.head <$> R.unions [eFilterUserEdit]
  bUserEdit <- R.stepper Nothing $ Unsafe.head <$> R.unions [eUserEdit]

  return $
    UserEnv.UserEnv
      { bDatabaseUser = bDatabaseUser,
        bUserCreate = bUserCreate,
        bUserCreateForm = bUserCreateForm,
        bUserLoginForm = bUserLoginForm,
        bUserLogin = bUserLogin,
        bSelectionUser = bSelectionUser,
        bFilterUser = bFilterUser,
        bUserDelete = bUserDelete,
        -- Edit
        bSelectionUserEdit = bSelectionUserEdit,
        bFilterUserEdit = bFilterUserEdit,
        bUserEdit = bUserEdit
      }

loanEnvSetup :: (MonadIO m, Read.MonadRead m (Db.Database Loan.Loan)) => Config.Config -> LoanCreate.Create -> m Env.LoanEnv
loanEnvSetup config loanCreate = do
  databaseLoan <- Read.read (Config.datastoreLoan config)
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

roleEnvSetup :: (MonadIO m, Read.MonadRead m (Db.Database Role.Role)) => Config.Config -> R.Event (Db.Database Role.Role) -> m Env.RoleEnv
roleEnvSetup config e = do
  databaseRole <- Read.read (Config.datastoreRole config)
  bDatabaseRole <- R.stepper (fromRight Db.empty databaseRole) $ Unsafe.head <$> R.unions [e]
  return $ Env.RoleEnv {bDatabaseRole = bDatabaseRole}

privilegeEnvSetup ::
  ( MonadIO m,
    Read.MonadRead m (Db.Database Privilege.Privilege)
  ) =>
  Config.Config ->
  R.Event (Db.Database Privilege.Privilege) ->
  m Env.PrivilegeEnv
privilegeEnvSetup config e = do
  databasePrivilege <- Read.read (Config.datastorePrivilege config)
  bDatabasePrivilege <- R.stepper (fromRight Db.empty databasePrivilege) $ Unsafe.head <$> R.unions [e]
  return $ Env.PrivilegeEnv {bDatabasePrivilege = bDatabasePrivilege}

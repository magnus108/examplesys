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
import qualified Data.Barbie as Barbie
import qualified Data.Functor.Product as Product
import Data.Generic.HKD
import qualified Data.Generic.HKD as HKD
import qualified Data.Time.Clock as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import Piece.App.UserEnv (UserEnv (bUserCreate, bUserCreateForm))
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Config as Config
import qualified Piece.Core.Form.FormDataExpr as Form
import qualified Piece.Core.Item as Item
import qualified Piece.Core.ItemCreateForm as ItemCreateForm
import qualified Piece.Core.ItemEditForm as ItemEditForm
import qualified Piece.Core.Loan as Loan
import qualified Piece.Core.LoanCreateForm as LoanCreateForm
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Tab as Tab
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Core.UserEditForm as UserEditForm
import qualified Piece.Core.UserLoginForm as UserLoginForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Item as Item
import qualified Piece.Db.Token as Token
import qualified Piece.Db.User as User
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Write as Write
import qualified Piece.Gui.Item.Create as ItemCreate
import qualified Piece.Gui.Item.Edit as ItemEdit
import qualified Piece.Gui.Item.List as ItemList
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

      itemList <- ItemList.setup env
      itemCreate <- ItemCreate.setup env
      itemEdit <- ItemEdit.setup env

      let tabViews =
            [ UI.getElement loanCreate,
              UI.getElement userCreate,
              UI.getElement userLogin,
              UI.getElement userList,
              UI.getElement userEdit,
              UI.getElement itemList,
              UI.getElement itemCreate,
              UI.getElement itemEdit
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

        let tUserCreatingForm = UserCreate.tUserCreatingForm userCreate
            eUserCreatingForm = UI.rumors tUserCreatingForm

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

        -- ITEM
        let tItemFilter = ItemList.tItemFilter itemList
            eItemFilter = R.rumors tItemFilter

        let tItemSelect = ItemList.tItemSelect itemList
            eItemSelect = R.rumors tItemSelect

        let eItemDelete = ItemList.eItemDelete itemList

        let tItemCreateForm = ItemCreate.tItemCreateForm itemCreate
            eItemCreateForm = R.rumors tItemCreateForm

        let eItemCreate = ItemCreate.eItemCreate itemCreate

        -- Edit

        let tItemEditFilter = ItemEdit.tItemFilter itemEdit
            eItemEditFilter = R.rumors tItemEditFilter

        let tItemEditSelect = ItemEdit.tItemSelect itemEdit
            eItemEditSelect = R.rumors tItemEditSelect

        let eItemEditForm = ItemEdit.eItemEditForm itemEdit

        let eItemEdit = ItemEdit.eItemEdit itemEdit

        ---- EDIT
        let tUserFilterEdit = UserEdit.tUserFilter userEdit
            eUserFilterEdit = R.rumors tUserFilterEdit

        let tUserSelectionEdit = UserEdit.tUserSelection userEdit
            eUserSelectionEdit = R.rumors tUserSelectionEdit

        let tUserEditKeyValue = UserEdit.tUserEditKeyValue userEdit
            eUserEditKeyValue = R.rumors tUserEditKeyValue

        let tUserEditForm = UserEdit.tUserEditForm userEdit
            eUserEditForm = UI.rumors tUserEditForm

        let tUserEditingForm = UserEdit.tUserEditingForm userEdit
            eUserEditingForm = UI.rumors tUserEditingForm

        -- LOAN CREATE
        let tLoanCreateItemFilter = LoanCreate.tItemFilter loanCreate
            eLoanCreateItemFilter = R.rumors tLoanCreateItemFilter

        let tLoanCreateUserFilter = LoanCreate.tUserFilter loanCreate
            eLoanCreateUserFilter = R.rumors tLoanCreateUserFilter

        let tLoanCreateForm = LoanCreate.tLoanCreateForm loanCreate
            eLoanCreateForm = R.rumors tLoanCreateForm

        let tLoanCreate = LoanCreate.tLoanCreate loanCreate
            eLoanCreate = R.rumors tLoanCreate

        loanEnv <- loanEnvSetup config eLoanCreateUserFilter eLoanCreateItemFilter eLoanCreateForm eLoanCreate

        timeEnv <- timeEnvSetup config eTime
        userEnv <- userEnvSetup config (Unsafe.head <$> R.unions [eUserCreateForm, eUserCreatingForm]) eUserCreate eUserLoginForm eUserLogin eTime eUserSelect eUserFilter eUserDelete {-edit-} eUserSelectionEdit eUserFilterEdit (Unsafe.head <$> R.unions [eUserEditForm, eUserEditingForm]) eUserEditKeyValue
        tokenEnv <- tokenEnvSetup config eUserLogin eTime

        roleEnv <- roleEnvSetup config R.never
        privilegeEnv <- privilegeEnvSetup config R.never
        tabEnv <- tabEnvSetup config R.never eSelectionTab

        itemEnv <- itemEnvSetup config eItemFilter eItemSelect eItemDelete eItemCreateForm eItemCreate eItemEditForm eItemEdit eItemEditFilter eItemEditSelect

        let env =
              Env.Env
                { tabEnv = tabEnv,
                  timeEnv = timeEnv,
                  loanEnv = loanEnv,
                  roleEnv = roleEnv,
                  userEnv = userEnv,
                  tokenEnv = tokenEnv,
                  privilegeEnv = privilegeEnv,
                  itemEnv = itemEnv
                }

        return env

      -- RETURN
      return ()

listen ::
  ( MonadReader env m,
    Has.Has Env.LoanEnv env,
    Has.Has Env.ItemEnv env,
    Has.Has UserEnv.UserEnv env,
    Has.Has Env.TabEnv env,
    Has.Has Env.TokenEnv env,
    Write.MonadWrite m (Db.Database Item.Item),
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

  itemEnv <- Has.grab @Env.ItemEnv
  let bDatabaseItem = Env.bDatabaseItem itemEnv

  UnliftIO.withRunInIO $ \run -> do
    _ <- UI.liftIO $ R.onChange bDatabaseLoan $ \s -> run $ void $ Write.write (Config.datastoreLoan config) s
    _ <- UI.liftIO $ R.onChange bDatabaseTab $ \s -> run $ void $ Write.write (Config.datastoreTab config) s
    _ <- UI.liftIO $ R.onChange bDatabaseUser $ \s -> run $ void $ Write.write (Config.datastoreUser config) s
    _ <- UI.liftIO $ R.onChange bDatabaseToken $ \s -> run $ void $ Write.write (Config.datastoreToken config) s
    _ <- UI.liftIO $ R.onChange bDatabaseItem $ \s -> run $ void $ Write.write (Config.datastoreItem config) s
    return ()

timeEnvSetup :: (Time.MonadTime m, MonadIO m) => Config.Config -> R.Event Time.Time -> m Env.TimeEnv
timeEnvSetup config eTime = do
  time <- Time.currentTime
  bTime <- R.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTime]
  return $
    Env.TimeEnv
      { Env.bTime = bTime
      }

tabEnvSetup :: (Read.MonadRead m (Db.Database Tab.Tab), MonadIO m) => Config.Config -> R.Event (Db.Database Tab.Tab) -> R.Event (Maybe Db.DatabaseKey) -> m Env.TabEnv
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
userCreateSetup eUserCreate = R.stepper Nothing $ Unsafe.head <$> R.unions [eUserCreate]

userLoginSetup :: MonadIO m => R.Event (Maybe Db.DatabaseKey) -> m (R.Behavior (Maybe Db.DatabaseKey))
userLoginSetup eUserLogin = R.stepper Nothing $ Unsafe.head <$> R.unions [eUserLogin]

userCreateFormSetup :: (MonadIO m) => R.Event UserCreateForm.User -> R.Event (Maybe User.User) -> m (R.Behavior UserCreateForm.User)
userCreateFormSetup eUserCreateForm eUserCreate = R.stepper (UserCreateForm.form "" "" False) $ Unsafe.head <$> R.unions [eUserCreateForm, UserCreateForm.form "" "" False <$ eUserCreate]

userLoginFormSetup :: MonadIO m => R.Event UserLoginForm.User -> R.Event (Maybe Db.DatabaseKey) -> m (R.Behavior (Maybe UserLoginForm.User))
userLoginFormSetup eUserLoginForm eUserLogin = R.stepper Nothing $ Unsafe.head <$> R.unions [Just <$> eUserLoginForm, Nothing <$ eUserLogin]

userEditFormSetup :: (Env.WithUserEnv env m, MonadIO m, Fix.MonadFix m) => R.Event (Maybe Db.DatabaseKey) -> R.Event UserEditForm.User -> R.Event (Maybe (Db.DatabaseKey, User.User)) -> m (R.Behavior UserEditForm.User)
userEditFormSetup eSelectUserEdit eUserEditForm eUserEditKeyValue = mdo
  bLookup <- User.lookup
  bUserEditForm <-
    R.stepper UserEditForm.emptyForm $
      Unsafe.head
        <$> R.unions
          [ eUserEditForm,
            UserEditForm.emptyForm <$ eUserEditKeyValue,
            (\x y -> UserEditForm.fromUser x (deconstruct @Identity y)) <$> bUserEditForm UI.<@> UI.filterJust ((=<<) <$> bLookup UI.<@> eSelectUserEdit)
          ]
  return bUserEditForm

databaseUserSetup :: (Fix.MonadFix m, MonadIO m, Read.MonadRead m (Db.Database User.User)) => Config.Config -> R.Event User.User -> R.Event (Db.DatabaseKey, User.User) -> R.Event (Maybe Db.DatabaseKey) -> m (R.Behavior (Db.Database User.User))
databaseUserSetup config eUserCreate eUserEdit eUserDelete = mdo
  databaseUser <- Read.read (Config.datastoreUser config)
  bDatabaseUser <-
    R.stepper (fromRight Db.empty databaseUser) $
      Unsafe.head
        <$> R.unions
          [ flip Db.create <$> bDatabaseUser UI.<@> eUserCreate,
            (\d (k, v) -> Db.update k v d) <$> bDatabaseUser UI.<@> eUserEdit,
            flip Db.delete <$> bDatabaseUser UI.<@> R.filterJust eUserDelete
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
selectionTokenSetup = R.stepper (Just 0) $ Unsafe.head <$> R.unions []

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
  R.Event UserEditForm.User ->
  R.Event (Maybe (Db.DatabaseKey, User.User)) ->
  m UserEnv.UserEnv
userEnvSetup config eUserCreateForm eUserCreate eUserLoginForm eUserLogin eTime eSelectUser eFilterUser eUserDelete eSelectUserEdit eFilterUserEdit eUserEditForm eUserEditKeyValue = mdo
  bUserCreateForm <- userCreateFormSetup eUserCreateForm eUserCreate
  bUserCreate <- userCreateSetup eUserCreate
  bUserLoginForm <- userLoginFormSetup eUserLoginForm eUserLogin
  bUserLogin <- userLoginSetup eUserLogin
  bDatabaseUser <- databaseUserSetup config (R.filterJust eUserCreate) (R.filterJust eUserEditKeyValue) eUserDelete

  bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions [eSelectUser, Nothing <$ eUserDelete]
  bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions [eFilterUser]
  bUserDelete <- R.stepper Nothing $ Unsafe.head <$> R.unions [eUserDelete]

  bSelectionUserEdit <- R.stepper Nothing $ Unsafe.head <$> R.unions [eSelectUserEdit, Nothing <$ eUserEditKeyValue]
  bFilterUserEdit <- R.stepper "" $ Unsafe.head <$> R.unions [eFilterUserEdit]
  bUserEditKeyValue <- R.stepper Nothing $ Unsafe.head <$> R.unions [eUserEditKeyValue]

  bUserEditForm <- userEditFormSetup eSelectUserEdit eUserEditForm eUserEditKeyValue

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
        bUserEditKeyValue = bUserEditKeyValue,
        bUserEditForm = bUserEditForm
      }

loanEnvSetup ::
  (Fix.MonadFix m, MonadIO m, Read.MonadRead m (Db.Database Loan.Loan)) =>
  Config.Config ->
  R.Event String ->
  R.Event String ->
  R.Event LoanCreateForm.Loan ->
  R.Event (Maybe Loan.Loan) ->
  m Env.LoanEnv
loanEnvSetup config eUserFilter eItemFilter eLoanCreateForm eLoanCreate = mdo
  databaseLoan <- Read.read (Config.datastoreLoan config)

  bDatabaseLoan <-
    R.stepper (fromRight Db.empty databaseLoan) $
      Unsafe.head
        <$> R.unions
          [flip Db.create <$> bDatabaseLoan UI.<@> R.filterJust eLoanCreate]

  bLoanCreateUserFilter <- R.stepper "" $ Unsafe.head <$> R.unions [eUserFilter]
  bLoanCreateItemFilter <- R.stepper "" $ Unsafe.head <$> R.unions [eItemFilter]

  bLoanCreateForm <-
    R.stepper (HKD.build @Loan.Loan (Form.Form Nothing Form.SelectExpr) (Form.Form Nothing Form.SelectExpr)) $
      Unsafe.head
        <$> R.unions
          [eLoanCreateForm]

  return $
    Env.LoanEnv
      { bDatabaseLoan = bDatabaseLoan,
        bLoanCreateUserFilter = bLoanCreateUserFilter,
        bLoanCreateItemFilter = bLoanCreateItemFilter,
        bLoanCreateForm = bLoanCreateForm
      }

itemEnvSetup ::
  (Env.WithItemEnv env m, Fix.MonadFix m, MonadIO m, Read.MonadRead m (Db.Database Item.Item)) =>
  Config.Config ->
  R.Event String ->
  R.Event (Maybe Db.DatabaseKey) ->
  R.Event Item.Item ->
  R.Event ItemCreateForm.Item ->
  R.Event Item.Item ->
  R.Event ItemEditForm.Item ->
  R.Event Item.Item ->
  R.Event String ->
  R.Event (Maybe Db.DatabaseKey) ->
  m Env.ItemEnv
itemEnvSetup config eItemFilter eItemSelect eItemDelete eItemCreateForm eItemCreate eItemEditForm eItemEdit eItemEditFilter eItemEditSelect = mdo
  databaseItem <- Read.read (Config.datastoreItem config)

  bDatabaseItem <-
    R.stepper (fromRight Db.empty databaseItem) $
      Unsafe.head
        <$> R.unions
          -- her burde jeg måske ikke slå op i selected. dvs itemDeleete skal indeholde alt.
          [ flip Db.delete <$> bDatabaseItem UI.<@> R.filterJust (bSelectItem UI.<@ eItemDelete),
            flip Db.create <$> bDatabaseItem UI.<@> eItemCreate,
            (\db (k, v) -> Db.update k v db) <$> bDatabaseItem UI.<@> R.filterJust (liftA2 (,) <$> bItemEditSelect UI.<@> (Just <$> eItemEdit))
          ]

  -- Delete
  bFilterItem <- R.stepper "" $ Unsafe.head <$> R.unions [eItemFilter, "" <$ eItemDelete]

  bSelectItem <-
    R.stepper Nothing $
      Unsafe.head
        <$> R.unions
          [ eItemSelect,
            -- fair nok jeg sletter slection ved delete og filter men lidt uheldigt det kommer 2 gange
            Nothing <$ eItemDelete,
            Nothing <$ eItemFilter
          ]

  bLookup <- Item.lookup

  bItemDeleteForm <-
    R.stepper (HKD.build @Item.Item Nothing) $
      Unsafe.head
        <$> R.unions
          -- her må jeg ikke smide id væk
          [ deconstruct <$> R.filterJust ((=<<) <$> bLookup UI.<@> eItemSelect),
            -- fair nok jeg sletter deleteform ved delete og filter men lidt uheldigt det kommer 2 gange
            mempty <$ eItemDelete,
            mempty <$ eItemFilter
          ]

  -- Create

  bItemCreateForm <-
    R.stepper (HKD.build @Item.Item (Compose Nothing)) $
      Unsafe.head <$> R.unions [eItemCreateForm, HKD.build @Item.Item (Compose Nothing) <$ eItemCreate]

  -- EDIT

  bItemEditFilter <- R.stepper "" $ Unsafe.head <$> R.unions [eItemEditFilter]

  bItemEditSelect <-
    R.stepper Nothing $
      Unsafe.head
        <$> R.unions
          [ eItemEditSelect
          ]

  bItemEditForm <-
    R.stepper (HKD.build @Item.Item (ItemEditForm.Form Nothing Form.StringExpr)) $
      Unsafe.head
        <$> R.unions
          [ eItemEditForm,
            Barbie.bmap (ItemEditForm.Form Nothing . ItemEditForm.to) <$> bItemEditForm UI.<@ eItemEdit,
            Barbie.bzipWith (\f i -> ItemEditForm.Form (Just (ItemEditForm.to f (runIdentity i))) (ItemEditForm.to f)) <$> bItemEditForm R.<@> (HKD.deconstruct @Identity <$> R.filterJust ((=<<) <$> bLookup UI.<@> eItemEditSelect))
          ]

  return $
    Env.ItemEnv
      { bDatabaseItem = bDatabaseItem,
        bSelectItem = bSelectItem,
        bFilterItem = bFilterItem,
        bItemDeleteForm = bItemDeleteForm,
        bItemCreateForm = bItemCreateForm,
        bItemEditForm = bItemEditForm,
        bItemEditSelect = bItemEditSelect,
        bItemEditFilter = bItemEditFilter
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

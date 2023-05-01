{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

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
import qualified Piece.Config as Config
import qualified Piece.Core.Time as Time (time, unTime)
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Write as Write
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Piece.Gui.Tab.Tab as Tab
import qualified Piece.Gui.Time.Time as GuiTime
import qualified Piece.Gui.User.Create as UserCreate
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
        UI.jsWindowReloadOnDisconnect = False
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
      content <- UI.string "bob"
      loanCreate <- LoanCreate.setup env
      userCreate <- UserCreate.setup env
      xx <- UI.div UI.# UI.sink UI.text (Time.formatTime Time.defaultTimeLocale "%F, %T" . Time.unTime <$> bTime)

      timeGui <- GuiTime.setup env bErr bSucc
      let tTimeGuiSucc = GuiTime.userText timeGui
          eTimeGuiSucc = UI.rumors tTimeGuiSucc
      let tTimeGuiErr = GuiTime.errText timeGui
          eTimeGuiErr = UI.rumors tTimeGuiErr

      bErr <- UI.stepper Nothing $ Unsafe.head <$> R.unions [Nothing <$ eTimeGuiSucc, eTimeGuiErr]
      bSucc <- UI.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTimeGuiSucc]

      tabs <- Tab.setup env
      _ <- UI.getBody window UI.#+ [UI.element tabs, UI.element xx, UI.element timeGui, UI.element userCreate]

      -- LISTEN
      _ <- UI.liftIOLater $ R.onChange bDatabaseLoan $ \s -> Monad.runApp env $ Write.write (Config.datastoreLoan config) s
      _ <- UI.liftIOLater $ R.onChange bDatabaseTab $ \s -> Monad.runApp env $ Write.write (Config.datastoreTab config) s
      _ <- UI.liftIOLater $ R.onChange bDatabaseUser $ \s -> Monad.runApp env $ Write.write (Config.datastoreUser config) s

      -- BEHAVIOR
      let tSelectionTab = Tab.tTabSelection tabs
          eSelectionTab = UI.rumors tSelectionTab

      let tLoanFilter = LoanCreate.tLoanFilter loanCreate
      let eLoanFilter = R.rumors tLoanFilter

      let tUserFormCreate = UserCreate.tUserFormCreate userCreate
          eUserFormCreate = UI.rumors tUserFormCreate
      let tUserCreate = UserCreate.tUserCreate userCreate
          eUserCreate = UI.rumors tUserCreate

      bUserFormCreate <- R.stepper Nothing $ Unsafe.head <$> R.unions [Just <$> eUserFormCreate, Nothing <$ eUserCreate]
      bUserCreate <- R.stepper Nothing $ Unsafe.head <$> R.unions [eUserCreate]

      bTTL <- R.stepper (Time.secondsToNominalDiffTime 100) $ Unsafe.head <$> R.unions []

      bTime <- R.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTime]

      bDatabaseTab <- R.stepper (fromRight Db.empty databaseTab) $ Unsafe.head <$> R.unions []
      bSelectionTab <- R.stepper (Just 0) $ Unsafe.head <$> R.unions [eSelectionTab]
      bViewMapTab <-
        R.stepper
          ( Map.fromList
              [ (0, UI.element loanCreate),
                (1, UI.element content)
              ]
          )
          $ Unsafe.head <$> R.unions []

      bDatabaseLoan <- R.stepper (fromRight Db.empty databaseLoan) $ Unsafe.head <$> R.unions []
      bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions []
      bSelectionItem <- R.stepper Nothing $ Unsafe.head <$> R.unions []
      bSelectionLoan <- R.stepper Nothing $ Unsafe.head <$> R.unions []
      bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions []
      bFilterItem <- R.stepper "" $ Unsafe.head <$> R.unions []
      bFilterLoan <- R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter]
      bModalState <- R.stepper False $ Unsafe.head <$> R.unions []

      bDatabaseRole <- R.stepper (fromRight Db.empty databaseRole) $ Unsafe.head <$> R.unions []
      bDatabaseUser <- R.stepper (fromRight Db.empty databaseUser) $ Unsafe.head <$> R.unions [flip Db.create <$> bDatabaseUser UI.<@> (R.filterJust eUserCreate)]
      bDatabasePrivilege <- R.stepper (fromRight Db.empty databasePrivilege) $ Unsafe.head <$> R.unions []

      validate <- liftIO $ Monad.runApp env $ Token.validate
      let eToken = validate UI.<@> eTime
          (eInvalidToken, eValidToken) = UI.split eToken
      bSelectionToken <- R.stepper Nothing $ Unsafe.head <$> R.unions [fmap Just eValidToken, Nothing <$ eInvalidToken]
      bDatabaseToken <- R.stepper (fromRight Db.empty databaseToken) $ Unsafe.head <$> R.unions []

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
                loanEnv =
                  Env.LoanEnv
                    { bDatabaseLoan = bDatabaseLoan,
                      bSelectionUser = bSelectionUser,
                      bSelectionItem = bSelectionItem,
                      bSelectionLoan = bSelectionLoan,
                      bFilterUser = bFilterUser,
                      bFilterItem = bFilterItem,
                      bFilterLoan = bFilterLoan,
                      bModalState = bModalState
                    },
                roleEnv = Env.RoleEnv {bDatabaseRole = bDatabaseRole},
                userEnv =
                  UserEnv.UserEnv
                    { bDatabaseUser = bDatabaseUser,
                      bUserCreate = bUserCreate,
                      bUserFormCreate = bUserFormCreate
                    },
                privilegeEnv = Env.PrivilegeEnv {bDatabasePrivilege = bDatabasePrivilege},
                tokenEnv =
                  Env.TokenEnv
                    { bDatabaseToken = bDatabaseToken,
                      bSelectionToken = bSelectionToken,
                      bTTL = bTTL
                    }
              }

      -- RETURN
      return ()

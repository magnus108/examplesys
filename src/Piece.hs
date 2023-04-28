{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import Control.Exception (try)
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Piece.App.Env as Env
import Piece.App.Monad (runApp)
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.Config as Config
import qualified Piece.Core.Time as Time (unTime)
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Effects.Token as Token
import qualified Piece.Effects.Write as Write
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Piece.Gui.Tab.Tab as Tab
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
      time <- liftIO $ Monad.runApp env $ Error.tryError Time.time
      eTime <- Time.timer env

      -- GUI
      content <- UI.string "bob"
      loanCreate <- LoanCreate.setup env
      xx <- UI.div UI.# UI.sink UI.text (Time.unTime <$> bTime)

      --------------------------
      mtime <- liftIO Time.getCurrentTime
      dd <- UI.entry (Time.formatTime Time.defaultTimeLocale "%F, %T" <$> bs)
      let tDd = UI.userText dd
      let ee = R.unsafeMapIO (try . Time.parseTimeM True Time.defaultTimeLocale "%F, %T") (UI.rumors tDd) :: UI.Event (Either SomeException Time.UTCTime)
      bs <- UI.stepper mtime $ Unsafe.head <$> R.unions [R.unsafeMapIO (\x -> return $ traceShow "gg" x) $ snd $ R.split ee]

      --------------------------

      -- TODO fixthislist
      tabs <- Tab.setup env
      _ <- UI.getBody window UI.#+ [UI.element tabs, UI.element xx, UI.element dd]

      -- LISTEN
      _ <- UI.liftIOLater $ R.onChange bDatabaseLoan $ \s -> Monad.runApp env $ Write.write (Config.datastoreLoan config) s
      _ <- UI.liftIOLater $ R.onChange bDatabaseTab $ \s -> Monad.runApp env $ Write.write (Config.datastoreTab config) s

      -- BEHAVIOR
      let tSelectionTab = Tab.tTabSelection tabs
          eSelectionTab = UI.rumors tSelectionTab

      let tLoanFilter = LoanCreate.tLoanFilter loanCreate
      let eLoanFilter = R.rumors tLoanFilter

      bTTL <- R.stepper (Time.secondsToDiffTime 100) $ Unsafe.head <$> R.unions []

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
      bDatabaseUser <- R.stepper (fromRight Db.empty databaseUser) $ Unsafe.head <$> R.unions []
      bDatabasePrivilege <- R.stepper (fromRight Db.empty databasePrivilege) $ Unsafe.head <$> R.unions []

      ----------------
      -- let ep = (,) <$> bSelectionToken UI.<@> eTime
      -- validate2 <- liftIO $ runApp env $ Token.validate2
      -- validate <- Token.validate env
      -- let validation = validate UI.<@> eTime
      bSelectionToken <- R.stepper Nothing $ Unsafe.head <$> R.unions [] -- [fmap Token.toID validation]
      bDatabaseToken <- R.stepper (fromRight Db.empty databaseToken) $ Unsafe.head <$> R.unions []
      ----------------

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
                    { bTime = bTime,
                      timeFormat = "%F, %T" -- TODO grim
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
                userEnv = Env.UserEnv {bDatabaseUser = bDatabaseUser},
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

{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as Time
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Timer as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.Config as Config
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Change as Change
import qualified Piece.Effects.Read as Read
import qualified Piece.Effects.Time as Time
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Piece.Gui.Tab.Tab as Tab
import qualified Piece.Time.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import qualified UnliftIO

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

      -- TIMER
      time <- liftIO $ Monad.runApp env $ Error.tryError Time.time
      eTimer <- Time.timer env

      -- GUI
      content <- UI.string "bob"
      loanCreate <- LoanCreate.setup env

      xx <- UI.div UI.# UI.sink UI.text ((Time.formatTime Time.defaultTimeLocale "%F, %T") <$> bTime)
      -- TODO fixthislist
      tabs <- Tab.setup env [(0, UI.element loanCreate), (1, UI.element content), (2, UI.element content), (3, UI.element content)]
      _ <- UI.getBody window UI.#+ [UI.element tabs, UI.element xx]

      -- LISTEN
      _ <- UI.liftIOLater $ Monad.runApp env $ Change.listen (Config.datastoreLoan config) bDatabaseLoan
      _ <- UI.liftIOLater $ Monad.runApp env $ Change.listen (Config.datastoreTab config) bDatabaseTab

      -- BEHAVIOR
      let eCreate = LoanCreate.eCreate loanCreate

      let tLoanDatabase = LoanCreate.tDatabaseLoan loanCreate
      let eLoanDatabase = R.rumors tLoanDatabase

      let tLoanFilter = LoanCreate.tLoanFilter loanCreate
      let eLoanFilter = R.rumors tLoanFilter

      bTime <- R.stepper (Unsafe.fromJust (rightToMaybe time)) $ Unsafe.head <$> R.unions [eTimer]

      bDatabaseTab <- R.stepper (fromRight Db.empty databaseTab) $ Unsafe.head <$> R.unions []

      bDatabaseLoan <- R.stepper (fromRight Db.empty databaseLoan) $ Unsafe.head <$> R.unions [eLoanDatabase]
      bSelectionUser <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
      bSelectionItem <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
      bSelectionLoan <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
      bFilterUser <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
      bFilterItem <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
      bFilterLoan <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter, "coco" <$ eCreate]
      bModalState <- UI.liftUI $ R.stepper False $ Unsafe.head <$> R.unions []

      -- ENV
      let env =
            Env.Env
              { tabEnv = Env.TabEnv {bDatabaseTab = bDatabaseTab},
                timeEnv = Env.TimeEnv {bTime = bTime},
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
                    }
              }

      -- RETURN
      return ()

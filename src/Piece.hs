{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.Config as Config
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Change as Change
import qualified Piece.Effects.Read as Read
import qualified Piece.Gui.Loan.Create as LoanCreate
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

      -- GUI
      content <- UI.string "bob"
      loanCreate <- LoanCreate.setup env
      _ <- UI.getBody window UI.#+ [UI.element content, UI.element loanCreate]

      -- LISTEN
      _ <- UI.liftIOLater $ Monad.runApp env $ Change.listen (Config.datastoreLoan config)

      -- BEHAVIOR
      let eCreate = LoanCreate.eCreate loanCreate

      let tLoanDatabase = LoanCreate.tDatabaseLoan loanCreate
      let eLoanDatabase = R.rumors tLoanDatabase

      let tLoanFilter = LoanCreate.tLoanFilter loanCreate
      let eLoanFilter = R.rumors tLoanFilter

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
              { loanEnv =
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

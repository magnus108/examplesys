module Piece
  ( main,
  )
where

import Control.Monad.Fix
import Graphics.UI.Threepenny.Core ((#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import Piece.App.Monad (AppEnv, runApp)
import qualified Piece.Config as Config
import Piece.Core.Loan (Loan)
import qualified Piece.Db.Db as Db
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
        UI.jsCustomHTML = Just "index.html"
      }
    $ \window -> void $ do
      liftIO $ mfix (\env -> runApp env $ app window config)

app ::
  forall m env.
  (MonadIO m, MonadFix m, Env.WithLoanEnv env m) =>
  UI.Window ->
  Config.Config ->
  m AppEnv
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Db.readJson datastoreLoan :: m (Db.Database Loan)

  -- GUI
  lol <- LoanCreate.setup window
  content <- liftIO $ UI.runUI window $ UI.string "lola"
  _ <- liftIO $ UI.runUI window $ UI.getBody window #+ [UI.element content]

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  -- BEHAVIOR
  bDatabaseLoan <- R.stepper databaseLoan $ Unsafe.head <$> R.unions [eLoanDatabase]
  bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- R.stepper "" $ Unsafe.head <$> R.unions []
  bModalState <- R.stepper False $ Unsafe.head <$> R.unions []

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

  -- CHANGES
  liftIO $ UI.runUI window $ UI.onChanges bDatabaseLoan $ Db.writeJson datastoreLoan

  return env

{-# LANGUAGE RecursiveDo #-}

module Piece2
  ( main,
  )
where

import qualified Control.Monad.Fix as MFix
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env2 as Env
import qualified Piece.App.Monad2 as Monad
import qualified Piece.Config as Config
import qualified Piece.Effects.Change2 as Change
import qualified Piece.Gui.Loan.Create2 as LoanCreate
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
    $ \window -> void $ mdo
      result <- MFix.mfix (\env -> Monad.runApp (Unsafe.fromJust (rightToMaybe env)) $ app window config)

      whenLeft_ result $ \err -> void $ do
        UI.getBody window UI.#+ [UI.string (show err)]

app ::
  ( UI.MonadUI m,
    MonadIO m,
    MFix.MonadFix m,
    MonadReader Env.AppBehavior m,
    Change.MonadRead m,
    Change.MonadChanges m
  ) =>
  UI.Window ->
  Config.Config ->
  m Env.AppBehavior
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Change.read datastoreLoan

  -- GUI
  env <- ask -- (undefined :: m Env.AppBehavior)
  result <- liftIO $ UI.runUI window $ Monad.runApp env $ LoanCreate.setup
  loanCreate <- LoanCreate.setup
  _ <- UI.liftUI $ UI.getBody window UI.#+ [UI.element loanCreate]

  -- LISTEN
  _ <- Change.listen datastoreLoan

  -- BEHAVIOR
  let eCreate = LoanCreate.eCreate loanCreate

  let tLoanDatabase = LoanCreate.tDatabaseLoan loanCreate
  let eLoanDatabase = R.rumors tLoanDatabase

  let tLoanFilter = LoanCreate.tLoanFilter loanCreate
  let eLoanFilter = R.rumors tLoanFilter

  bDatabaseLoan <- UI.liftUI $ R.stepper databaseLoan $ Unsafe.head <$> R.unions [eLoanDatabase]
  bSelectionUser <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter, "coco" <$ eCreate]
  bModalState <- UI.liftUI $ R.stepper False $ Unsafe.head <$> R.unions []

  -- ENV
  let env =
        Env.AppBehavior $
          Env.LoanBehavior
            { _bDatabaseLoan = bDatabaseLoan,
              _bSelectionUser = bSelectionUser,
              _bSelectionItem = bSelectionItem,
              _bSelectionLoan = bSelectionLoan,
              _bFilterUser = bFilterUser,
              _bFilterItem = bFilterItem,
              _bFilterLoan = bFilterLoan,
              _bModalState = bModalState
            }

  return env

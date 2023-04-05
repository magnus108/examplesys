{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import qualified Data.ByteString as UI
import Graphics.UI.Threepenny.Core ((#), (#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import Piece.App.Monad (AppEnv, runApp)
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
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
      mfix (\env -> runApp env $ app window config)

app ::
  forall m env.
  (UI.MonadUI m, MonadIO m, MonadFix m, Env.WithLoanEnv env m) =>
  UI.Window ->
  Config.Config ->
  m AppEnv
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Db.readJson datastoreLoan :: m (Db.Database Loan.Loan)

  -- GUI
  lol <- LoanCreate.setup window
  content <- liftIO $ UI.runUI window $ UI.string "lola"
  _ <- liftIO $ UI.runUI window $ mdo
    createBtn <- Elements.button #+ [UI.string "Creater"]
    bDatabase <-
      R.stepper databaseLoan $
        Unsafe.head
          <$> R.unions
            [ Db.create (Loan.Loan "bob") <$> bDatabase R.<@ (Events.click createBtn)
            ] -- eLoanDatabase]
    gg <- Elements.div # UI.sink items ((\x -> fmap (UI.string . Loan.name) (Db.elems x)) <$> bDatabase)
    UI.getBody window #+ [UI.element createBtn, UI.element content, UI.element gg, UI.element lol]

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  let tLoanFilter = LoanCreate.tLoanFilter lol
  let eLoanFilter = R.rumors tLoanFilter

  -- BEHAVIOR
  bDatabaseLoan <- R.stepper databaseLoan $ Unsafe.head <$> R.unions [eLoanDatabase]
  bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter]
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

items = UI.mkWriteAttr $ \i x -> void $ do
  return x # UI.set UI.children [] #+ map (\i -> Elements.div #+ [i]) i

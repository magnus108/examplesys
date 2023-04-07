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
import Piece.App.Monad (withRunInUI)
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Change as Change
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
      let enva =
            Env.Env
              { loanEnv = undefined,
                window = window
              }
      liftIO $
        mfix
          ( \env -> Monad.runApp enva $ withRunInUI $ \runInUI -> do
              content <- UI.string "lola"
              UI.getBody window #+ [UI.element content]

              _ <- runInUI $ Change.listen "lol"
              return env
          )

type WithDefaults env m = (Change.MonadChanges m, Change.MonadRead m, Env.WithLoanEnv env m)

app ::
  (UI.MonadUI m, WithDefaults env m, MonadIO m, MonadFix m) =>
  UI.Window ->
  Config.Config ->
  m Monad.AppEnv
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Change.read datastoreLoan

  -- GUI
  traceShowM "fucekr"
  content <- UI.liftUI $ UI.string "lola"
  traceShowM "fucekrsssss"
  _ <- UI.liftUI $ mdo
    createBtn <- Elements.button #+ [UI.string "Creater"]
    bDatabase <-
      R.stepper databaseLoan $
        Unsafe.head
          <$> R.unions
            [ Db.create (Loan.Loan "bob") <$> bDatabase R.<@ (Events.click createBtn)
            ] -- eLoanDatabase]
    gg <- Elements.div # UI.sink items ((\x -> fmap (UI.string . Loan.name) (Db.elems x)) <$> bDatabase)
    UI.getBody window #+ [UI.element createBtn, UI.element content, UI.element gg]

  -- EVENTS
  let eDatabaseLoan = R.unions []

  -- BEHAVIOR
  bDatabaseLoan <- R.stepper databaseLoan $ Unsafe.head <$> eDatabaseLoan
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
                  eDatabaseLoan = eDatabaseLoan,
                  bSelectionUser = bSelectionUser,
                  bSelectionItem = bSelectionItem,
                  bSelectionLoan = bSelectionLoan,
                  bFilterUser = bFilterUser,
                  bFilterItem = bFilterItem,
                  bFilterLoan = bFilterLoan,
                  bModalState = bModalState
                },
            window = window
          }

  -- CHANGES
  -- _ <- Change.listen datastoreLoan

  return env

items = UI.mkWriteAttr $ \i x -> void $ do
  return x # UI.set UI.children [] #+ map (\i -> Elements.div #+ [i]) i

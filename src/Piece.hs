{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import qualified Data.ByteString as UI
import GHC.IO (unsafeInterleaveIO)
import Graphics.UI.Threepenny.Core ((#), (#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
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
      traceShowM "a"
      content <- liftIO $ mdo
        (env, content) <- Monad.runApp env $ app window config
        return content
      traceShowM "b"
      content2 <- UI.liftUI $ UI.string "lola"
      content3 <- UI.liftUI $ UI.string "lola2"
      UI.getBody window # UI.set UI.children [content2, UI.getElement content, content3]

type WithDefaults env m = (Change.MonadChanges m, Change.MonadRead m, Env.WithLoanEnv env m)

app ::
  (UI.MonadUI m, WithDefaults env m, MonadIO m, MonadFix m) =>
  UI.Window ->
  Config.Config ->
  m (Monad.AppEnv, LoanCreate.Create)
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Change.read datastoreLoan

  -- GUI
  traceShowM "1"
  lol <- LoanCreate.setup window
  traceShowM "3"
  content2 <- UI.liftUI $ UI.string "lola"
  traceShowM "2"
  content <- UI.liftUI $ mdo
    traceShowM "3"
    createBtn <- Elements.button #+ [UI.string "Creater"]
    traceShowM "5"
    bDatabase <-
      R.stepper databaseLoan $
        Unsafe.head
          <$> R.unions
            [ Db.create (Loan.Loan "bob") <$> bDatabase R.<@ (Events.click createBtn)
            ] -- eLoanDatabase]
    gg <- Elements.div # UI.sink items ((\x -> fmap (UI.string . Loan.name) (Db.elems x)) <$> bDatabase)
    traceShowM "115"
    UI.getBody window #+ [UI.element createBtn, UI.element gg, UI.element content2]
  traceShowM "4"

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  traceShowM "8"
  let tLoanFilter = LoanCreate.tLoanFilter lol
  let eLoanFilter = R.rumors tLoanFilter

  -- EVENTS

  -- BEHAVIOR
  traceShowM "7"
  bDatabaseLoan <- UI.liftUI $ R.stepper databaseLoan $ Unsafe.head <$> R.unions [eLoanDatabase]
  traceShowM "9"
  bSelectionUser <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  traceShowM "14"
  bSelectionItem <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  traceShowM "13"
  bSelectionLoan <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  traceShowM "12"
  bFilterItem <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  traceShowM "11"
  bFilterLoan <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter]
  traceShowM "10"
  bModalState <- UI.liftUI $ R.stepper False $ Unsafe.head <$> R.unions []
  traceShowM "6"

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
                },
            window = window
          }

  -- CHANGES
  _ <- Change.listen datastoreLoan

  traceShowM "5"
  return (env, lol)

items = UI.mkWriteAttr $ \i x -> void $ do
  return x # UI.set UI.children [] #+ map (\i -> Elements.div #+ [i]) i

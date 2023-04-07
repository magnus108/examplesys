{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import Control.Concurrent (forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
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
  messages <- Chan.newChan
  UI.startGUI
    UI.defaultConfig
      { UI.jsPort = Just port,
        UI.jsStatic = Just "./static",
        UI.jsCustomHTML = Just "index.html",
        UI.jsCallBufferMode = UI.NoBuffering
      }
    $ \window -> void $ do
      loanEnv <- newEmptyMVar
      chan <- newMVar messages
      liftIO $ do
        let env =
              Env.Env
                { loanEnv = loanEnv,
                  chan = chan
                }
        -- READ
        databaseLoan <- Db.readJson (Config.datastoreLoan config)
        Monad.runApp env $ app databaseLoan
        return ()

      UI.liftUI $ mdo
        content <- UI.string "lola"
        createBtn <- Elements.button #+ [UI.string "Creater"]
        bDatabase <-
          R.stepper (Db.empty) $
            Unsafe.head
              <$> R.unions
                [ Db.create (Loan.Loan "bob") <$> bDatabase R.<@ (Events.click createBtn)
                ] -- eLoanDatabase]
        gg <- Elements.div # UI.sink items ((\x -> fmap (UI.string . Loan.name) (Db.elems x)) <$> bDatabase)
        UI.getBody window #+ [UI.element createBtn, UI.element content, UI.element gg]

      messageReceiver <- liftIO $ forkIO $ receiveMessages window messages

      UI.on UI.disconnect window $ const $ liftIO $ do
        killThread messageReceiver

-- ligegyldigt
receiveMessages w msgs = do
  messages <- Chan.getChanContents msgs
  forM_ messages $ \msg -> do
    UI.runUI w $ do
      msg
      UI.flushCallBuffer

type WithDefaults env m = (Change.MonadChanges m, Change.MonadRead m, Env.WithLoanEnv env m)

app ::
  (UI.MonadUI m, WithDefaults env m, MonadIO m, MonadFix m) =>
  Db.Database Loan.Loan ->
  m ()
app databaseLoan = do
  -- GUI
  lol <- LoanCreate.setup

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  let tLoanFilter = LoanCreate.tLoanFilter lol
  let eLoanFilter = R.rumors tLoanFilter

  -- EVENTS
  let eDatabaseLoan = R.unions [eLoanDatabase]

  -- BEHAVIOR
  bDatabaseLoan <- R.stepper databaseLoan $ Unsafe.head <$> eDatabaseLoan
  bSelectionUser <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter]
  bModalState <- R.stepper False $ Unsafe.head <$> R.unions []

  -- ENV
  loanEnv <- Has.grab @(MVar (Env.LoanEnv))
  liftIO $
    putMVar loanEnv $
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
        }

  -- CHANGES
  -- _ <- Change.listen datastoreLoan

  return ()

items = UI.mkWriteAttr $ \i x -> void $ do
  return x # UI.set UI.children [] #+ map (\i -> Elements.div #+ [i]) i

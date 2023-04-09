{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import qualified Control.Concurrent.Chan as Chan
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
      mfix (\(env) -> Monad.runApp env $ app window config)

type WithDefaults env m = (Change.MonadChanges m, Change.MonadRead m, Env.WithLoanEnv env m)

app :: (WithDefaults env m, MonadFix m, UI.MonadUI m) => UI.Window -> Config.Config -> m (Monad.AppEnv)
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Change.read datastoreLoan

  -- GUI
  lol <- LoanCreate.setup

  _ <- UI.liftUI $ UI.getBody window #+ [UI.element lol]

  let eCreate = LoanCreate.eCreate lol

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  let tLoanFilter = LoanCreate.tLoanFilter lol
  let eLoanFilter = R.rumors tLoanFilter

  -- BEHAVIOR
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

  return env

{-
      -- ENV
      (env, content) <- liftIO $ mdo
        (env, content) <- Monad.runApp env $ app config messages window
        return (env, content)

      -- CHANGES
      listen <- liftIO $ Monad.runApp env $ do
        Change.listen window messages (Config.datastoreLoan config)

      traceShowM "b"
      content2 <- UI.liftUI $ UI.string "lola"
      content3 <- UI.liftUI $ UI.string "lola2"
      UI.getBody window # UI.set UI.children [content2, UI.getElement content, content3]

      messageReceiver <- liftIO $ forkIO $ receiveMessages window messages

      UI.on UI.disconnect window $ const $ liftIO $ do
        killThread messageReceiver

type WithDefaults env m = (Change.MonadChanges m, Change.MonadRead m, Env.WithLoanEnv env m)

receiveMessages :: UI.Window -> Chan (IO ()) -> IO ()
receiveMessages w msgs = do
  messages <- Chan.getChanContents msgs
  forM_ messages $ \msg -> do
    a <- msg
    return a

--- USE THROW
--- USE CHAN

runM :: (WithDefaults env m, MonadIO m, MonadFix m) => Chan (IO ()) -> UI.Window -> UI.UI a -> m a
runM chan window ui = do
  i <- liftIO $ UI.runUI window ui
  liftIO $ writeChan chan $ do
    let a = (const ()) $! i
    return a
  return i

app2 :: (WithDefaults env m, MonadIO m, MonadFix m) => Config.Config -> Chan (IO ()) -> UI.Window -> m (Monad.AppEnv, UI.Element)
app2 config chan window = do
  traceShowM "sssahater"
  content <- runM chan window $ UI.string "loladskolad"
  traceShowM "skater"
  let env = undefined
  return (env, content)

app :: (WithDefaults env m, MonadIO m, MonadFix m) => Config.Config -> Chan (IO ()) -> UI.Window -> m (Monad.AppEnv, LoanCreate.Create)
app Config.Config {..} chan window = do
  -- READ
  databaseLoan <- Change.read datastoreLoan

  -- GUI
  traceShowM "1"
  lol <- LoanCreate.setup chan window
  traceShowM "2"
  content2 <- runM chan window $ UI.string "lola"
  content <- runM chan window $ mdo
    createBtn <- Elements.button #+ [UI.string "Creater"]
    bDatabase <-
      R.stepper databaseLoan $
        Unsafe.head
          <$> R.unions
            [ Db.create (Loan.Loan "bob") <$> bDatabase R.<@ (Events.click createBtn)
            ] -- eLoanDatabase]
    gg <- Elements.div # UI.sink items ((\x -> fmap (UI.string . Loan.name) (Db.elems x)) <$> bDatabase)
    UI.getBody window #+ [UI.element createBtn, UI.element gg, UI.element content2]

  let eCreate = LoanCreate.eCreate lol

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  let tLoanFilter = LoanCreate.tLoanFilter lol
  let eLoanFilter = R.rumors tLoanFilter

  -- EVENTS

  -- BEHAVIOR
  bDatabaseLoan <- runM chan window $ R.stepper databaseLoan $ Unsafe.head <$> R.unions [eLoanDatabase]
  bSelectionUser <- runM chan window $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- runM chan window $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- runM chan window $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- runM chan window $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- runM chan window $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- runM chan window $ R.stepper "" $ Unsafe.head <$> R.unions [eLoanFilter, "coco" <$ eCreate]
  bModalState <- runM chan window $ R.stepper False $ Unsafe.head <$> R.unions []

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
  return (env, lol)

items = UI.mkWriteAttr $ \i x -> void $ do
  return x # UI.set UI.children [] #+ map (\i -> Elements.div #+ [i]) i
  -}

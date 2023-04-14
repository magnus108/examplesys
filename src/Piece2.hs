{-# LANGUAGE RecursiveDo #-}

module Piece2
  ( main,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM as STM
import qualified Control.Lens.Operators as LOperators
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Fix as MFix
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env2 as Env
import qualified Piece.App.Error2 as Error
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
    $ \window -> void $ do
      result <- MFix.mfix (\ ~(Right env) -> Monad.runApp env $ app window config)

      whenLeft_ result $ \err -> void $ do
        UI.getBody window UI.#+ [UI.string (show err)]

app ::
  ( UI.MonadUI m,
    MonadIO m,
    MFix.MonadFix m,
    MonadReader r m,
    Env.HasLoanBehavior r,
    Change.MonadRead m
  ) =>
  UI.Window ->
  Config.Config ->
  m Env.AppBehavior
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Change.read datastoreLoan
  f <- Change.go
  -- Except.throwError $ (Error._NotFound LOperators.#) ()

  -- GUI
  loanCreate <- LoanCreate.setup

  _ <- UI.liftUI $ UI.getBody window UI.#+ [UI.element loanCreate]

  -- LISTEN
  -- _ <- Change.listen ""

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

  -- t <- timer -- UI.# (liftUI UI.set interval 1000)
  -- eBeat <- UI.liftUI $ R.accumE (0 :: Int) $ (\beat -> (beat + 1)) <$ tick t
  -- UI.liftUI $ UI.onEvent (eBeat) $ \beat -> do
  -- traceShowM ("lol" ++ (show beat))
  -- start t

  return env

data Timer = Timer
  { tRunning :: GetSet Bool Bool,
    tInterval :: GetSet Int Int, -- in ms
    tTick :: UI.Event ()
  }

-- | Create a new timer
timer ::
  ( Monad m,
    MonadIO m,
    Except.MonadError e m,
    Error.AsUserError e
  ) =>
  m Timer
timer = do
  tvRunning <- newTVarIO False
  tvInterval <- newTVarIO 1000
  (tTick, fire) <- liftIO UI.newEvent

  Except.throwError $ (Error._NotFound LOperators.#) ()
  liftIO $ forkIO $ forever $ do
    STM.atomically $ do
      b <- readTVar tvRunning
      traceShowM b
      when (not b) STM.retry
    wait <- STM.atomically $ readTVar tvInterval
    fire ()
    threadDelay (wait * 5000)

  let tRunning = fromTVar tvRunning
      tInterval = fromTVar tvInterval

  return $ Timer {..}

-- | Timer event.
tick :: Timer -> UI.Event ()
tick = tTick

-- | Timer interval in milliseconds.
interval :: UI.Attr Timer Int
interval = fromGetSet tInterval

-- | Whether the timer is running or not.
running :: UI.Attr Timer Bool
running = fromGetSet tRunning

-- | Start the timer.
start :: (Monad m, UI.MonadUI m) => Timer -> m ()
start = UI.liftUI . UI.set' running True

-- | Stop the timer.
stop :: (Monad m, UI.MonadUI m) => Timer -> m ()
stop = UI.liftUI . UI.set' running False

fromTVar :: TVar a -> GetSet a a
fromTVar var = (STM.atomically $ readTVar var, STM.atomically . writeTVar var)

type GetSet i o = (IO o, i -> IO ())

fromGetSet :: (x -> GetSet i o) -> UI.ReadWriteAttr x i o
fromGetSet f = UI.mkReadWriteAttr (liftIO . fst . f) (\i x -> liftIO $ snd (f x) i)

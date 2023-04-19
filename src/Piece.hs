{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Piece
  ( main,
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Control.Lens.Operators as LOperators
import Control.Monad.Base
import qualified Control.Monad.Except as Except
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad.Fix as MFix
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO), UnliftIO (unliftIO), wrappedWithRunInIO)
import Control.Monad.Trans.Control
  ( ComposeSt,
    MonadBaseControl (..),
    MonadTransControl (..),
    control,
    defaultLiftBaseWith,
    defaultRestoreM,
  )
import qualified Control.Monad.Trans.RWS.Lazy as Monadd
import Graphics.UI.Threepenny (askWindow)
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env as Env
import Piece.App.Error (AppError (..), As (..), UserError (..), WithError)
import qualified Piece.App.Error as Error
import qualified Piece.App.Monad as Monad
import Piece.App.Monad3
import Piece.CakeSlayer.Error (catchError, throwError, tryError)
import qualified Piece.CakeSlayer.Monad as CakeSlayer
import qualified Piece.Config as Config
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Change as Change
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (mask_, try)

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
    $ \window ->
      void $ do
        --        haha <- ss (UI.string "gaga") (pure ())
        -- dd <- liftIO $ focks2 $ return ()
        --      liftIO $ control $ \runIn ->
        --       runIn $ lol
        undefined

{-
      result <-
        liftIO $
          MFix.mfix
            ( \env -> Monad.runApp env $ do
                xx <- CakeSlayer.withRunInUI window $ \f -> UI.string "gaga"
                --                loanCreate <- LoanCreate.setup window
                _ <- CakeSlayer.withRunInUI window $ \f -> UI.getBody window UI.#+ [UI.element xx]

                t <- tryError $ timer -- UI.# (liftUI UI.set interval 1000)
                CakeSlayer.withRunInUI window $ \f -> case t of
                  Left _ -> do
                    traceShowM "gg"
                    return ()
                  Right t' -> start t'

                -- BEHAVIOR
                bDatabaseLoan <- R.stepper Db.empty $ Unsafe.head <$> R.unions []
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

                return env
            )
      return ()

-}
--      whenLeft_ result $ \err -> void $ do
--       UI.getBody window UI.#+ [UI.string (show err)]

type WithDefaults env m = ({-Change.MonadChanges m,-} Change.MonadRead m, Env.WithLoanEnv env m)

app2 :: (MonadUnliftIO m, WithDefaults env m, MonadIO m, MonadFix m, Error.WithError err m, Error.As err Error.AppError) => UI.Window -> Config.Config -> m Monad.AppEnv
app2 window Config.Config {..} = do
  databaseLoan <- return Db.empty -- Change.read datastoreLoan

  -- GUI
  xx <- withRunInIO $ \_ -> UI.runUI window $ UI.string "gg"
  -- loanCreate <- LoanCreate.setup window
  _ <- withRunInIO $ \_ -> UI.runUI window $ UI.getBody window UI.#+ [UI.element xx]

  -- BEHAVIOR
  bDatabaseLoan <- R.stepper databaseLoan $ Unsafe.head <$> R.unions []
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

  return env

{-
app :: (WithDefaults env m, MonadFix m, Error.WithError err m, Error.As err Error.AppError) => UI.Window -> Config.Config -> m Monad.AppEnv
app window Config.Config {..} = do
  -- READ
  databaseLoan <- Change.read datastoreLoan
  -- Except.throwError $ (Error._NotFound LOperators.#) ()

  -- GUI
  -- loanCreate <- LoanCreate.setup

  -- _ <- UI.liftUI $ UI.getBody window UI.#+ [UI.element loanCreate]

  -- LISTEN
  -- _ <- Change.listen ""

  -- BEHAVIOR
  -- let eCreate = LoanCreate.eCreate loanCreate

  -- let tLoanDatabase = LoanCreate.tDatabaseLoan loanCreate
  -- let eLoanDatabase = R.rumors tLoanDatabase

  -- let tLoanFilter = LoanCreate.tLoanFilter loanCreate
  -- let eLoanFilter = R.rumors tLoanFilter

  bDatabaseLoan <- UI.liftUI $ R.stepper databaseLoan $ Unsafe.head <$> R.unions [] -- eLoanDatabase]
  bSelectionUser <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions [] -- eLoanFilter, "coco" <$ eCreate]
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

  -- t <- timer -- UI.# (liftUI UI.set interval 1000)
  -- eBeat <- UI.liftUI $ R.accumE (0 :: Int) $ (\beat -> (beat + 1)) <$ tick t
  -- UI.liftUI $ UI.onEvent (eBeat) $ \beat -> do
  -- traceShowM ("lol" ++ (show beat))
  -- start t

  return env
  -}

data Timer = Timer
  { tRunning :: GetSet Bool Bool,
    tInterval :: GetSet Int Int, -- in ms
    tTick :: UI.Event ()
  }

-- | Create a new timer
timer ::
  ( Monad m,
    MonadIO m,
    MonadUnliftIO m,
    As err UserError,
    WithError err m
  ) =>
  m Timer
timer = do
  tvRunning <- newTVarIO False
  tvInterval <- newTVarIO 1000
  (tTick, fire) <- liftIO UI.newEvent

  mask_ $ throwError (as NotFound)

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

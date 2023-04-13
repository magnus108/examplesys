{-# LANGUAGE RecursiveDo #-}

module Piece
  ( main,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Monad.Catch as NU
import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Trans.Writer.Lazy
import qualified Data.ByteString as UI
import GHC.IO (unsafeInterleaveIO)
import Graphics.UI.Threepenny.Core ((#), (#+))
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as Elements
import qualified Graphics.UI.Threepenny.Events as Events
import qualified Graphics.UI.Threepenny.Widgets as Widgets
import qualified Piece.App.Env as Env
import qualified Piece.App.Error as Error
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as E
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Monad as CakeSlayer
import qualified Piece.Config as Config
import qualified Piece.Core.Loan as Loan
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Change as Change
import qualified Piece.Gui.Loan.Create as LoanCreate
import qualified Piece2
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import UnliftIO.Exception

{-
catchApp :: Monad.AppEnv -> UI.UI Monad.AppEnv
catchApp env = Monad.runApp env (app window config `catchError` handleException)
        where
            handleException undefined

main2 :: UI.UI ()
main2 = do
  env <- mdo
    env' <- catchApp env
    return env'

-}
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
    $ \window -> do
      void $ do
        z <-
          UI.liftUI $
            withMonoidAggregate (window, config) lola
        return ()

lola _ [] = undefined
lola (window, config) l@(~((e, ae) : xs)) = WriterT $ do
  y <- Monad.runApp ae $ E.tryError $ app window config
  let ll = case y of
        Left _ -> []
        Right (x, y) -> undefined
  return (y, l)

-- case z of
-- Left _ -> do
--  bob <- UI.liftUI $ UI.string "ups"
-- void $ UI.getBody window #+ [UI.element bob]
-- Right (e, v) -> do
-- bob <- UI.liftUI $ UI.string "ok"
-- void $ UI.getBody window #+ [UI.element e]

withMonoidAggregate :: (MonadFix m) => (UI.Window, Config.Config) -> ((UI.Window, Config.Config) -> [(UI.Element, Monad.AppEnv)] -> WriterT [(UI.Element, Monad.AppEnv)] m r) -> m r
withMonoidAggregate args f = mdo
  (output, s) <- runWriterT (f args s)
  return output

type WithDefaults env m = ({-Change.MonadChanges m,-} Change.MonadRead m, Env.WithLoanEnv env m)

app :: (WithDefaults env m, MonadFix m, UI.MonadUI m, Error.WithError err m, Error.As err Error.AppError) => UI.Window -> Config.Config -> m (UI.Element, Monad.AppEnv)
app window Config.Config {..} = do
  -- READ
  traceShowM "lola"
  databaseLoan <- Change.read datastoreLoan
  traceShowM "lola2"

  -- GUI
  -- databaseLoan <- withRunInIO $ \x -> do
  -- Db.readJson datastoreLoan :: IO (Db.Database Loan.Loan)

  lol <- LoanCreate.setup

  -- LISTEN
  --  _ <- Change.listen "" `E.catchError` (\x -> return ())

  -- BEHAVIOR
  let eCreate = LoanCreate.eCreate lol

  let tLoanDatabase = LoanCreate.tDatabaseLoan lol
  let eLoanDatabase = R.rumors tLoanDatabase

  let tLoanFilter = LoanCreate.tLoanFilter lol
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

  return (UI.getElement lol, env)

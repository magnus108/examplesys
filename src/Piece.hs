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
          withMonoidAggregate
            ( \s -> StateT $ \x -> Monad.runApp s $ do
                traceShowM "lol"
                y <- E.tryError $ app window config
                traceShowM "lol2"
                return (y, x)
            )
        traceShowM ("r" ++ (show (isRight z)))
        traceShowM (isLeft z)
        when (isLeft z) $ do
          bob <- UI.liftUI $ UI.string "ups"
          void $ UI.getBody window #+ [UI.element bob]

        return ()

withMonoidAggregate :: (UI.MonadUI m, MonadFix m) => (Monad.AppEnv -> StateT Monad.AppEnv m r) -> m r
withMonoidAggregate f = mdo
  bDatabaseLoan <- UI.liftUI $ R.stepper Db.empty $ Unsafe.head <$> R.unions []
  bSelectionUser <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionItem <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bSelectionLoan <- UI.liftUI $ R.stepper Nothing $ Unsafe.head <$> R.unions []
  bFilterUser <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterItem <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
  bFilterLoan <- UI.liftUI $ R.stepper "" $ Unsafe.head <$> R.unions []
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

  (output, s) <- runStateT (f s) env
  return output

{-

        traceShowM "g"
        let ~(Right r) = x
        traceShowM "g3"
        x <- Monad.runApp r $ E.tryError $ app window config
        traceShowM "g2"
        if isLeftt x
          then do
            traceShowM "g4"
            bob <- UI.liftUI $ UI.string "ikups"
            traceShowM "g5"
            UI.getBody window #+ [UI.element bob]
          else do
            traceShowM "g6"
            bob <- UI.liftUI $ UI.string "ups"
            traceShowM "g7"
            UI.getBody window #+ [UI.element bob]

isLeftt :: Either a b -> Bool
isLeftt (~(Left _)) = True
isLeftt (~(Right _)) = False

isRightt :: Either a b -> Bool
isRightt (~(Left _)) = False
isRightt (~(Right _)) = True
    -}

{-
  UI.liftUI $ mdo
env <- Monad.runApp (fst env) $ do
  e' <- E.tryError $ app window config
  case e' of
    Left _ -> do
      bob <- UI.liftUI $ UI.string "ups"
      return (error "fuck", UI.getBody window #+ [UI.element bob])
    Right e -> do
      bob <- UI.liftUI $ UI.string "ups"
      return (e, UI.getBody window #+ [UI.element bob])
return env
-}

-- case runIt of
-- Left y -> do
--  bob <- UI.liftUI $ UI.string "ups"
-- void $ UI.liftUI $ UI.getBody window #+ [UI.element bob]
-- Right y -> return y

-- case dataWrite of
--   Left y -> x (throwError (as NotFound))
--  Right y -> return y
--          `E.catchE` ( \(e :: SomeException) -> void $ do
---w                            bob <- UI.liftUI $ UI.string "ups"
--                         UI.liftUI $ UI.getBody window #+ [UI.element bob]
--                     )

--            return env `NU.catch` \(e :: E.AppException Error.AppError) -> gvoid $ do

type WithDefaults env m = (Change.MonadChanges m, Change.MonadRead m, Env.WithLoanEnv env m)

app :: (WithDefaults env m, MonadFix m, UI.MonadUI m, Error.WithError err m, Error.As err Error.AppError) => UI.Window -> Config.Config -> m (Monad.AppEnv)
app window Config.Config {..} = do
  -- READ
  traceShowM "lola"
  databaseLoan <- Change.read ""
  traceShowM "lola2"

  -- GUI
  -- databaseLoan <- withRunInIO $ \x -> do
  -- Db.readJson datastoreLoan :: IO (Db.Database Loan.Loan)

  lol <- LoanCreate.setup
  _ <- UI.liftUI $ UI.getBody window #+ [UI.element lol]

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

  return env

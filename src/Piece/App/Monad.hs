module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    runAppAsIO,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import GHC.IO (unsafeInterleaveIO)
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.App.Env (Env, window)
import Piece.App.Error (AppError)
import Piece.CakeSlayer.Error (ErrorWithSource)
import qualified Piece.CakeSlayer.Monad as CakeSlayer
import UnliftIO (MonadUnliftIO)

type AppEnv = Env App

newtype App a = App
  { unApp :: CakeSlayer.App AppError AppEnv a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader AppEnv,
      MonadError (ErrorWithSource AppError)
    )

instance MonadFix App where
  mfix f = do
    var <- liftIO newEmptyMVar
    ans <- liftIO $ unsafeInterleaveIO $ takeMVar var
    result <- f ans
    putMVar var result
    return result

instance UI.MonadUI App where
  liftUI ui = do
    var <- liftIO newEmptyMVar
    ans <- liftIO $ unsafeInterleaveIO $ takeMVar var
    let f x =
          mfix
            ( \ ~(y, window) -> do
                UI.runUI window $ do
                  w <- UI.askWindow
                  return (y, w)
            )
    (result, w) <- liftIO $ f ans
    putMVar var result
    return result

--      CakeSlayer.App $ ReaderT $ \env -> do
--   CakeSlayer.runApp env $ do
--    traceShowM "324"
---   liftIO $ do
--   traceShowM "3241"
--  gg <- UI.runUI (window env) ui
-- traceShowM "32411"
-- return gg

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

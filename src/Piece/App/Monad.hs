module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    runAppAsIO,
  )
where

import Control.Monad.Except (MonadError)
import qualified Control.Monad.Fix as Fix
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.App.Env (Env, window)
import Piece.App.Error (AppError)
import Piece.CakeSlayer (ErrorWithSource)
import qualified Piece.CakeSlayer.Monad as CakeSlayer
import System.IO.Unsafe
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
      MonadFail,
      MonadUnliftIO,
      MonadReader AppEnv,
      MonadError (ErrorWithSource AppError),
      Fix.MonadFix
    )

instance UI.MonadUI App where
  liftUI ui = App $ CakeSlayer.App $ ReaderT $ \env -> unsafeInterleaveIO $ UI.runUI (window env) ui

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

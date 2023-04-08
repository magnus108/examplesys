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
      MonadError (ErrorWithSource AppError),
      MonadFix,
      UI.MonadUI
    )

instance MonadFix (CakeSlayer.App err env) where
  mfix f = CakeSlayer.App $ mfix (CakeSlayer.unApp . f)

instance UI.MonadUI (CakeSlayer.App err AppEnv) where
  liftUI ui = CakeSlayer.App $ ReaderT $ \env -> do
    CakeSlayer.runApp env $ do
      liftIO $ unsafeInterleaveIO $ UI.runUI (window env) ui

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

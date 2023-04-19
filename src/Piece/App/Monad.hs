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
import Piece.App.Env (Env)
import Piece.App.Error (AppError)
import Piece.CakeSlayer (ErrorWithSource)
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
      Fix.MonadFix,
      CakeSlayer.MonadUnliftUI
    )

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    runAppAsUI,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM, try)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.App.Env (Env)
import Piece.App.Error (AppError)
import Piece.CakeSlayer.Error (ErrorWithSource)
import qualified Piece.CakeSlayer.Monad as CakeSlayer

type AppEnv = Env App

newtype App a = App
  { unApp :: CakeSlayer.App AppError AppEnv a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppEnv,
      MonadError (ErrorWithSource AppError),
      MonadFix,
      UI.MonadUI,
      MonadUnliftIO,
      MonadThrow,
      MonadCatch
    )

runApp :: AppEnv -> App a -> UI.UI a
runApp env = CakeSlayer.runApp env . unApp

runAppAsUI :: AppEnv -> App a -> UI.UI (Either (ErrorWithSource AppError) a)
runAppAsUI env = CakeSlayer.runAppAsUI env . unApp

module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    runAppAsM,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM, try)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.App.Env (Env)
import Piece.App.Error (AppError)
import qualified Piece.App.Monad2
import Piece.CakeSlayer.Error (ErrorWithSource)
import qualified Piece.CakeSlayer.Monad as CakeSlayer

type AppEnv m = Env (App m)

newtype App m a = App
  { unApp :: CakeSlayer.App AppError (AppEnv m) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (AppEnv m),
      MonadError (ErrorWithSource AppError),
      MonadFix,
      MonadThrow,
      MonadCatch
    )

instance CakeSlayer.MonadUnliftUILater (App UI.UI)

instance UI.MonadUI (App UI.UI)

runApp :: AppEnv m -> App m a -> m a
runApp env = CakeSlayer.runApp env . unApp

runAppAsM :: MonadCatch m => AppEnv m -> App m a -> m (Either (ErrorWithSource AppError) a)
runAppAsM env = CakeSlayer.runAppAsM env . unApp

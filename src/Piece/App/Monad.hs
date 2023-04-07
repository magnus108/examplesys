module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    runAppAsIO,
  )
where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.App.Env (Env)
import Piece.App.Error (AppError)
import Piece.CakeSlayer.Error (ErrorWithSource)
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Monad as CakeSlayer
import UnliftIO (MonadUnliftIO, withRunInIO)

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
      MonadFix,
      MonadError (ErrorWithSource AppError)
    )

instance UI.MonadUI App where
  liftUI ui = do
    chan <- Has.grab @(MVar (Chan.Chan (UI.UI ())))
    x <- liftIO $ readMVar chan
    liftIO $ do
      Chan.writeChan x $ do
        ui
        return ()
    liftIO empty

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

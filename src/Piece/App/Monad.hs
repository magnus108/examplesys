module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    runAppAsIO,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Piece.App.Env (Env)
import Piece.App.Error (AppError)
import Piece.CakeSlayer (ErrorWithSource)
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
      MonadUnliftIO,
      MonadReader AppEnv,
      MonadFix,
      MonadError (ErrorWithSource AppError)
    )

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

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
import Piece.App.Env (Env)
import Piece.App.Error (AppError)
import Piece.CakeSlayer.Error (ErrorWithSource)
import qualified Piece.CakeSlayer.Has as Has
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

runApp :: AppEnv -> App a -> UI.UI a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> UI.UI (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsUI env . unApp

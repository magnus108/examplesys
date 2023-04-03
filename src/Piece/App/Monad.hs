module Piece.App.Monad
    ( App (..)
    , AppEnv
    , runApp
    , runAppAsIO
    ) where

import CakeSlayer (ErrorWithSource)
import Control.Monad.Except (MonadError)
import UnliftIO (MonadUnliftIO)

import Piece.App.Env (Env)
import Piece.App.Error (AppError)

import qualified CakeSlayer.Monad as CakeSlayer


type AppEnv = Env App

newtype App a = App
    { unApp :: CakeSlayer.App AppError AppEnv a
    } deriving newtype
        ( Functor, Applicative, Monad, MonadIO, MonadUnliftIO
        , MonadReader AppEnv, MonadError (ErrorWithSource AppError)
        )

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

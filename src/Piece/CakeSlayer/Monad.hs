module Piece.CakeSlayer.Monad
  ( App (..),
    runApp,
    runAppAsUI,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM, try)
import Control.Monad.Except (MonadError (..), MonadFix)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import GHC.IO.Exception (userError)
import Graphics.UI.Threepenny (MonadUI (..), UI, askWindow, runUI)
import Piece.CakeSlayer.Error (AppException (..), ErrorWithSource)
import Relude.Extra.Bifunctor (firstF)

newtype App (err :: Type) env a = App
  { unApp :: ReaderT env UI a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadUnliftIO,
      MonadReader env,
      MonadIO,
      MonadFix
    )

instance MonadFail (App err env) where
  fail err = liftIO $ throwIO $ userError err

instance MonadUI (App err env) where
  liftUI = App . lift . liftUI

-- Not sure about this
instance MonadUnliftIO UI where
  withRunInIO inner = do
    window <- askWindow
    liftIO $ inner $ runUI window

instance
  (Show err, Typeable err) =>
  MonadError (ErrorWithSource err) (App err env)
  where
  throwError :: ErrorWithSource err -> App err env a
  throwError = throwM . AppException
  {-# INLINE throwError #-}

  catchError ::
    App err env a ->
    (ErrorWithSource err -> App err env a) ->
    App err env a
  catchError action handler = App $ ReaderT $ \env -> do
    let ioAction = runApp env action
    ioAction `catch` \(AppException e) -> runApp env $ handler e
  {-# INLINE catchError #-}

runApp :: env -> App err env a -> UI a
runApp env = usingReaderT env . unApp
{-# INLINE runApp #-}

runAppAsUI ::
  (Show err, Typeable err) =>
  env ->
  App err env a ->
  UI (Either (ErrorWithSource err) a)
runAppAsUI env = firstF unAppException . try . runApp env
{-# INLINE runAppAsUI #-}

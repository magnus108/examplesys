module Piece.CakeSlayer.Monad
  ( App (..),
    runApp,
    runAppAsM,
    MonadUnliftUILater (..),
  )
where

import Control.Exception (throwIO)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM, try)
import Control.Monad.Except (MonadError (..), MonadFix)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import GHC.IO.Exception (userError)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Graphics.UI.Threepenny (MonadUI (..), UI, Window, askWindow, liftIOLater, runUI)
import Piece.CakeSlayer.Error (AppException (..), ErrorWithSource)
import Relude.Extra.Bifunctor (firstF)

newtype App (err :: Type) env m a = App
  { unApp :: ReaderT env m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadReader env,
      MonadIO,
      MonadFix
    )

class MonadUnliftUILater m where
  withRunInUILater :: ((m () -> UI ()) -> UI ()) -> m ()

instance MonadUnliftUILater UI where
  withRunInUILater inner =
    inner $ \x -> do
      window <- askWindow
      liftIOLater $ runUI window x

instance MonadUnliftUILater (App err env UI) where
  withRunInUILater inner = App $
    ReaderT $ \r ->
      withRunInUILater $ \run -> do
        inner (run . runApp r)

instance MonadThrow m => MonadFail (App err env m) where
  fail err = throwM $ userError err

instance MonadUI (App err env UI) where
  liftUI = App . lift . liftUI

instance
  (Show err, Typeable err, MonadCatch m) =>
  MonadError (ErrorWithSource err) (App err env m)
  where
  throwError :: ErrorWithSource err -> App err env m a
  throwError = throwM . AppException
  {-# INLINE throwError #-}

  catchError ::
    App err env m a ->
    (ErrorWithSource err -> App err env m a) ->
    App err env m a
  catchError action handler = App $ ReaderT $ \env -> do
    let ioAction = runApp env action
    ioAction `catch` \(AppException e) -> runApp env $ handler e
  {-# INLINE catchError #-}

runApp :: env -> App err env m a -> m a
runApp env = usingReaderT env . unApp
{-# INLINE runApp #-}

runAppAsM ::
  (Show err, Typeable err, MonadCatch m) =>
  env ->
  App err env m a ->
  m (Either (ErrorWithSource err) a)
runAppAsM env = firstF unAppException . try . runApp env
{-# INLINE runAppAsM #-}

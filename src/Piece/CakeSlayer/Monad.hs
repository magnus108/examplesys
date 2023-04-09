module Piece.CakeSlayer.Monad
  ( App (..),
    runApp,
    runAppAsUI,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Except (MonadError (..), MonadFix)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import GHC.IO.Exception (userError)
import Graphics.UI.Threepenny (MonadUI (..), UI)
import Piece.CakeSlayer.Error (AppException (..), ErrorWithSource)
import Relude.Extra.Bifunctor (firstF)
import UnliftIO.Exception (catch, try)

newtype App (err :: Type) env a = App
  { unApp :: ReaderT env UI a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadReader env,
      MonadIO,
      MonadUnliftIO,
      MonadFix,
      MonadUI
    )

instance MonadFail UI where
  fail err = liftIO $ throwIO $ userError err

instance MonadUnliftIO UI

instance (MonadUI m) => MonadUI (ReaderT r m) where
  liftUI = lift . liftUI

instance
  (Show err, Typeable err) =>
  MonadError (ErrorWithSource err) (App err env)
  where
  throwError :: ErrorWithSource err -> App err env a
  throwError = liftIO . throwIO . AppException
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

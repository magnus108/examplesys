{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.CakeSlayer.Monad
  ( App (..),
    runApp,
    runAppAsIO,
    MonadUnliftUI (..),
  )
where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Base
import Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Fix as Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Trans.Control (MonadBaseControl (..), control, defaultRestoreM)
import Control.Monad.Trans.Identity (IdentityT (IdentityT))
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.CakeSlayer.Error (AppException (..), ErrorWithSource)
import Relude.Extra.Bifunctor (firstF)

newtype App (err :: Type) env a = App
  { unApp :: ReaderT env IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadReader env,
      MonadIO,
      Fix.MonadFix,
      MonadUnliftIO,
      MonadUnliftUI
    )

class MonadIO m => MonadUnliftUI m where
  withRunInUI :: UI.Window -> ((forall a. m a -> UI.UI a) -> UI.UI b) -> m b

instance MonadUnliftUI UI.UI where
  withRunInUI w inner = inner id

instance MonadUnliftUI IO where
  withRunInUI w inner = do
    traceShowM "ui"
    UI.runUI w $ do
      traceShowM "ui"
      inner liftIO

instance MonadUnliftUI m => MonadUnliftUI (ReaderT r m) where
  withRunInUI w inner =
    ReaderT $ \r ->
      withRunInUI w $ \run ->
        inner (run . flip runReaderT r)

instance MonadUnliftUI m => MonadUnliftUI (IdentityT m) where
  withRunInUI w inner =
    IdentityT $
      withRunInUI w $ \run ->
        inner (run . runIdentityT)

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

runApp :: env -> App err env a -> IO a
runApp env = usingReaderT env . unApp
{-# INLINE runApp #-}

runAppAsIO ::
  (Show err, Typeable err) =>
  env ->
  App err env a ->
  IO (Either (ErrorWithSource err) a)
runAppAsIO env = firstF unAppException . try . runApp env
{-# INLINE runAppAsIO #-}

module Piece.CakeSlayer.Monad
  ( App (..),
    runApp,
    runAppAsIO,
  )
where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Fix
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
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
      MonadFix,
      MonadFail,
      MonadReader env,
      MonadIO,
      MonadUnliftIO
    )

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

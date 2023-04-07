{-# LANGUAGE RankNTypes #-}

module Piece.App.Monad
  ( App (..),
    AppEnv,
    runApp,
    withRunInUI,
    runAppAsIO,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import qualified Graphics.UI.Threepenny.Core as UI
import Piece.App.Env (Env, window)
import Piece.App.Error (AppError)
import Piece.CakeSlayer (ErrorWithSource)
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
      MonadFix,
      MonadUnliftIO,
      MonadReader AppEnv,
      MonadError (ErrorWithSource AppError)
    )

class UI.MonadIO m => MonadUnliftUI m where
  withRunInUI :: ((forall a. m a -> UI.UI a) -> UI.UI b) -> m b

instance MonadUnliftUI UI.UI where
  withRunInUI inner = inner id

instance MonadUnliftUI App where
  withRunInUI inner = do
    App $ CakeSlayer.App $ ReaderT $ \r -> UI.runUI (window r) $ inner (\x -> liftIO $ runApp r _)

--    liftIO $ UI.runUI window $ inner _

-- instance MonadUnliftUI App where
-- withRunInUI inner = do
--   window <- Has.grab @UI.Window
--  _

--    liftIO $ UI.runUI window $ inner _

{-
liftUI ui = do
  traceShowM "lol"
  window <- Has.grab @UI.Window
  traceShowM "lol2"
  liftIO $ do
    traceShowM "lola"
    d <- UI.runUI window $ do
      traceShowM "lola11"
      z <- ui
      traceShowM "lola11111111"
      return z
    traceShowM "lola11"
    return d
    -}

instance MonadUnliftUI m => MonadUnliftUI (ReaderT r m) where
  {-# INLINE withRunInUI #-}
  withRunInUI inner =
    ReaderT $ \r ->
      withRunInUI $ \run ->
        inner (run . flip runReaderT r)

{-
instance UI.MonadUI App where
  liftUI ui = do
    traceShowM "lol"
    window <- Has.grab @UI.Window
    traceShowM "lol2"
    liftIO $ do
      traceShowM "lola"
      d <- UI.runUI window $ do
        traceShowM "lola11"
        z <- ui
        traceShowM "lola11111111"
        return z
      traceShowM "lola11"
      return d
      -}

runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp

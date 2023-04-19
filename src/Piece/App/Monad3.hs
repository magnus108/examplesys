{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Piece.App.Monad3 where

import Control.Monad.Base
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Fix as Fix
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (StM, liftBaseWith, restoreM), MonadTransControl (StT, liftWith, restoreT), control, defaultLiftBaseWith, defaultLiftWith2, defaultRestoreM, defaultRestoreT2)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Config)
import qualified Piece.App.Env2 as Env
import qualified Piece.App.Error2 as Error

-- instance MonadBase UI.UI IO where
-- liftBase = undefined

-- | Config the app requires.
data Config = Config {cButtonText :: String}

-- | Monad the app runs in.
newtype Foo a = Foo {unFoo :: ReaderT Config IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      Fix.MonadFix
    )

newtype App m a = App
  { unApp :: ReaderT Env.AppBehavior (ExceptT Error.AppError m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env.AppBehavior,
      Except.MonadError Error.AppError,
      Fix.MonadFix
    )

instance MonadBase IO (App IO) where
  liftBase = App . liftBase

instance MonadBase UI.UI UI.UI where
  liftBase = id

instance MonadBase UI.UI (App UI.UI) where
  liftBase = App . liftBase

instance MonadBaseControl UI.UI UI.UI where
  type StM UI.UI a = a
  liftBaseWith f = f id
  restoreM = return

newtype CalcT m a = CalcT {unCalcT :: StateT Int (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadTrans)

instance MonadTransControl CalcT where
  type StT CalcT a = StT (ExceptT String) (StT (StateT Int) a)
  liftWith = defaultLiftWith2 CalcT unCalcT
  restoreT = defaultRestoreT2 CalcT

instance MonadBase b m => MonadBase b (CalcT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (CalcT m) where
  type StM (CalcT m) a = ComposeSt CalcT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

newtype CalcIO a = CalcIO {unCalcIO :: CalcT IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      --      MonadIO,
      --     Fix.MonadFix
      MonadBase IO,
      MonadBaseControl IO
    )

newtype CalcUI a = CalcUI {unCalcUI :: CalcT UI a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      --      MonadIO,
      --     Fix.MonadFix
      MonadBase UI.UI,
      MonadBaseControl UI.UI
    )

newtype Foos a = Foos {unFoos :: ReaderT Config (IdentityT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Fix.MonadFix
      --     MonadBase IO,
      --      MonadBaseControl IO
    )

-- | Run the Foo monad.
runFoo :: Foo a -> Config -> IO a
runFoo = runReaderT . unFoo

-- | Start the app.
start :: Int -> IO ()
start port =
  startGUI defaultConfig {jsPort = Just port} $
    \w -> liftIO $ runFoo (app w) (Config "Some Config")

-- | Our app in the Foo monad.
app :: Window -> Foo ()
app window = do
  -- buttonText <- cButtonText <$> ask
  --  button <- control $ \x -> x ask -- UI.button # set UI.text buttonText
  -- void $ liftBase $ getBody window #+ [element button]
  return ()

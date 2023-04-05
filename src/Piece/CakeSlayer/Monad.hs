module Piece.CakeSlayer.Monad
  ( App (..),
    runApp,
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
  { unApp :: ReaderT env UI.UI a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader env,
      MonadIO,
      MonadFix,
      UI.MonadUI
    )

instance (UI.MonadUI m) => UI.MonadUI (ReaderT r m) where
  liftUI = lift . UI.liftUI

runApp :: env -> App err env a -> UI.UI a
runApp env = usingReaderT env . unApp
{-# INLINE runApp #-}

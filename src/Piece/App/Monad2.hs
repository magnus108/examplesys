module Piece.App.Monad2
  ( App,
    runApp,
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Fix as Fix
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Piece.App.Env2 as Env
import qualified Piece.App.Error2 as Error

newtype App a = App
  { unApp :: ReaderT Env.AppBehavior (ExceptT Error.AppError IO) a
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

runApp :: Env.AppBehavior -> App a -> IO (Either Error.AppError a)
runApp env = Except.runExceptT . usingReaderT env . unApp

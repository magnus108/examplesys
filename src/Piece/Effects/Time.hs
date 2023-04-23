module Piece.Effects.Time
  ( MonadTime,
    time,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Time.LocalTime as Time
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified UnliftIO

class Monad m => MonadTime m where
  time :: m Time.ZonedTime

instance MonadTime Monad.App where
  time = timeImpl
  {-# INLINE time #-}

timeImpl :: (MonadUnliftIO m, E.As err E.UserError, E.WithError err m) => m Time.ZonedTime
timeImpl = do
  zonedTime <- UnliftIO.tryAny (liftIO Time.getZonedTime)
  case zonedTime of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

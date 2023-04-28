module Piece.Effects.Time
  ( MonadTime,
    time,
    MonadSerializeTime,
    MonadParseTime,
    parseTime,
    serializeTime,
  )
where

import qualified Data.Time as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as Time
import qualified Piece.App.Env as Env
import qualified Piece.App.Error as E
import qualified Piece.App.Monad as Monad
import qualified Piece.CakeSlayer.Error as Error
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Time as Time
import qualified UnliftIO

class Monad m => MonadTime m where
  time :: m Time.Time

instance MonadTime Monad.App where
  time = timeImpl
  {-# INLINE time #-}

timeImpl :: (MonadSerializeTime m, UnliftIO.MonadUnliftIO m, E.As err E.UserError, E.WithError err m) => m Time.Time
timeImpl = do
  time <- UnliftIO.tryAny (liftIO Time.getCurrentTime)
  case time of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> serializeTime y

class Monad m => MonadParseTime m where
  parseTime :: Time.Time -> m Time.UTCTime

instance MonadParseTime Monad.App where
  parseTime = parseTimeImpl
  {-# INLINE parseTime #-}

parseTimeImpl :: (Env.WithTimeEnv env m, UnliftIO.MonadUnliftIO m, MonadFail m, E.As err E.UserError, E.WithError err m) => Time.Time -> m Time.UTCTime
parseTimeImpl x = do
  timeEnv <- Has.grab @Env.TimeEnv
  parse <- UnliftIO.tryAny $ Time.parseTimeM True Time.defaultTimeLocale (Env.timeFormat timeEnv) (Time.unTime x)
  case parse of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

class Monad m => MonadSerializeTime m where
  serializeTime :: Time.UTCTime -> m Time.Time

instance MonadSerializeTime Monad.App where
  serializeTime = serializeTimeImpl
  {-# INLINE serializeTime #-}

serializeTimeImpl :: (Env.WithTimeEnv env m) => Time.UTCTime -> m Time.Time
serializeTimeImpl x = do
  timeEnv <- Has.grab @Env.TimeEnv
  return $ Time.time $ Time.formatTime Time.defaultTimeLocale (Env.timeFormat timeEnv) x

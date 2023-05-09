module Piece.Effects.Time
  ( MonadTime,
    currentTime,
    currentTimeImpl,
    MonadParseTime,
    parseTime,
    parseTimeImpl,
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
  currentTime :: m (Either () Time.Time)

instance MonadTime Monad.App where
  currentTime = currentTimeImpl
  {-# INLINE currentTime #-}

currentTimeImpl :: (UnliftIO.MonadUnliftIO m) => m (Either () Time.Time)
currentTimeImpl = do
  time <- UnliftIO.tryAny $ Time.time <$> liftIO Time.getCurrentTime
  case time of
    Left _ -> return (Left ())
    Right y -> return (Right y)

class Monad m => MonadParseTime m where
  parseTime :: String -> String -> m (Either () Time.Time)

instance MonadParseTime Monad.App where
  parseTime = parseTimeImpl
  {-# INLINE parseTime #-}

parseTimeImpl :: (Env.WithTimeEnv env m, UnliftIO.MonadUnliftIO m, MonadFail m) => String -> String -> m (Either () Time.Time)
parseTimeImpl format x = do
  parse <- UnliftIO.tryAny $ Time.time <$> Time.parseTimeM True Time.defaultTimeLocale format x
  case parse of
    Left _ -> return (Left ())
    Right y -> return (Right y)

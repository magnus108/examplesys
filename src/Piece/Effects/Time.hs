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
  currentTime :: m Time.Time

instance MonadTime Monad.App where
  currentTime = currentTimeImplSafe
  {-# INLINE currentTime #-}

currentTimeImpl :: (MonadIO m) => m Time.Time
currentTimeImpl = Time.time <$> liftIO Time.getCurrentTime

currentTimeImplSafe :: (UnliftIO.MonadUnliftIO m, E.As err E.UserError, E.WithError err m) => m Time.Time
currentTimeImplSafe = do
  time <- UnliftIO.tryAny currentTimeImpl
  case time of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

class Monad m => MonadParseTime m where
  parseTime :: String -> String -> m Time.Time

instance MonadParseTime Monad.App where
  parseTime = parseTimeImplSafe
  {-# INLINE parseTime #-}

parseTimeImpl :: (MonadFail m) => String -> String -> m Time.Time
parseTimeImpl x y = Time.time <$> Time.parseTimeM True Time.defaultTimeLocale x y

parseTimeImplSafe :: (Env.WithTimeEnv env m, UnliftIO.MonadUnliftIO m, MonadFail m, E.As err E.UserError, E.WithError err m) => String -> String -> m Time.Time
parseTimeImplSafe format x = do
  parse <- UnliftIO.tryAny $ parseTimeImpl format x
  case parse of
    Left _ -> Error.throwError (E.as E.NotFound)
    Right y -> return y

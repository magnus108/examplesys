module Piece.CakeSlayer.Error
  ( WithError,
    ErrorWithSource (..),
    throwError,
    tryError,
    catchError,
    liftError,
    AppException (..),
    toNoSourceException,
    throwOnNothing,
    throwOnNothingM,
    SourcePosition (..),
    toSourcePosition,
  )
where

import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as E (catchError, throwError)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))

type WithError err m = (MonadError (ErrorWithSource err) m, HasCallStack)

data ErrorWithSource err = ErrorWithSource
  { errorWithSourceCallStack :: !SourcePosition,
    errorWithSourceType :: !err
  }
  deriving stock (Show, Eq, Functor)

throwError :: WithError err m => err -> m a
throwError = E.throwError . ErrorWithSource (toSourcePosition callStack)
{-# INLINE throwError #-}

catchError :: WithError err m => m a -> (err -> m a) -> m a
catchError action handler = action `E.catchError` (handler . errorWithSourceType)
{-# INLINE catchError #-}

liftError :: WithError e m => Either e a -> m a
liftError = either throwError pure
{-# INLINE liftError #-}

tryError :: WithError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `E.catchError` (pure . Left . errorWithSourceType)
{-# INLINE tryError #-}

newtype SourcePosition = SourcePosition
  { unSourcePosition :: Text
  }
  deriving newtype (Show, Eq)

toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
      [] -> "<unknown loc>"
      [(name, loc)] -> showLoc name loc
      (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc {..} =
      toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

newtype AppException err = AppException
  { unAppException :: ErrorWithSource err
  }
  deriving stock (Show)
  deriving anyclass (Exception)

toNoSourceException :: err -> AppException err
toNoSourceException = AppException . ErrorWithSource (SourcePosition "<unknown loc>")
{-# INLINE toNoSourceException #-}

throwOnNothing :: WithError err m => err -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure
{-# INLINE throwOnNothing #-}

throwOnNothingM :: WithError err m => err -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err
{-# INLINE throwOnNothingM #-}

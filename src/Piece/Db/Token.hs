module Piece.Db.Token
  ( lookup,
    getTime,
    validate,
  )
where

import qualified Data.Time.Clock as Time
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Db as Db
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified UnliftIO as MonadUnliftIO

lookup :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Token.Token))
lookup = do
  tokenEnv <- Has.grab @Env.TokenEnv
  return $ flip Db.lookup <$> Env.bDatabaseToken tokenEnv

getTime :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Time.Time))
getTime = do
  bLookup <- lookup
  return $ (fmap Token.time .) <$> bLookup

-- TODO this is ugly. Move to event
validate :: (MonadUnliftIO.MonadUnliftIO m, MonadIO m, Time.MonadParseTime m, Env.WithTokenEnv env m) => m (R.Behavior (Time.Time -> Either () DB.DatabaseKey))
validate = do
  tokenEnv <- Has.grab @Env.TokenEnv
  let bTTL = Env.bTTL tokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv
  bGetTime <- getTime
  return $
    ( \f ttl tokenKey now -> case tokenKey of
        Nothing -> Left ()
        Just k ->
          let tokenTime = f k
           in case tokenTime of
                Nothing -> Left ()
                Just t ->
                  let diffTime = Time.diffUTCTime (Time.unTime now) (Time.unTime t)
                   in if ttl < diffTime then Left () else Right k
    )
      <$> bGetTime
      <*> bTTL
      <*> bSelectionToken

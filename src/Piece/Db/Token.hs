module Piece.Db.Token
  ( lookup,
    getTime,
    getPrivilege,
    validate,
  )
where

import qualified Data.Time.Clock as Time
import qualified Piece.App.Env as Env
import qualified Piece.App.UserEnv as UserEnv
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Privilege as Privilege
import qualified Piece.Db.Role as Role
import qualified Piece.Db.User as User
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified UnliftIO as MonadUnliftIO

lookup :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Token.Token))
lookup = do
  userEnv <- Has.grab @UserEnv.UserEnv
  return $ flip Db.lookup <$> UserEnv.bDatabaseToken userEnv

getTime :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Time.Time))
getTime = do
  bLookup <- lookup
  return $ (fmap Token.time .) <$> bLookup

getUser :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe User.User))
getUser = do
  bLookup <- lookup
  let bLookupUserId = (fmap Token.user .) <$> bLookup
  bLookupUser <- User.lookup
  return $ (>=>) <$> bLookupUserId <*> bLookupUser

getRoles :: (Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Db.DatabaseKey]))
getRoles = do
  bGetUser <- getUser
  return $ (maybe [1] User.roles .) <$> bGetUser

getRoles' :: (Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Role.Role]))
getRoles' = do
  bGetRoles <- getRoles
  bLookupRole <- Role.lookup
  return $ (\f g x -> catMaybes $ fmap f (g x)) <$> bLookupRole <*> bGetRoles

getPrivilege :: (Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Db.DatabaseKey]))
getPrivilege = do
  bGetRoles <- getRoles'
  return $ ((concat . fmap Role.privilege) .) <$> bGetRoles

-- TODO refac
validate :: (MonadIO m, Env.WithUserEnv env m) => m (R.Behavior (Time.Time -> Either () DB.DatabaseKey))
validate = do
  userEnv <- Has.grab @UserEnv.UserEnv
  let bTTL = UserEnv.bTTL userEnv
  let bSelectionToken = UserEnv.bSelectionToken userEnv
  bTokenTime <- getTime
  return $
    ( \f ttl tokenKey now -> case tokenKey of
        Nothing -> Left ()
        Just k ->
          let tokenTime = f k
           in case tokenTime of
                Nothing -> Left ()
                Just t ->
                  case ttl of
                    Nothing -> Right k
                    Just ttl' ->
                      let diffTime = Time.diffUTCTime (Time.unTime now) (Time.unTime t)
                       in if ttl' < diffTime then Left () else Right k
    )
      <$> bTokenTime
      <*> bTTL
      <*> bSelectionToken

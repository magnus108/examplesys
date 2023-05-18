module Piece.Db.Token
  ( lookup,
    getTime,
    getUserId,
    getUser,
    getRoleIds,
    getRoles,
    getPrivilege,
    lessThanTTL,
    createNow,
    validate,
  )
where

import Data.Profunctor
import qualified Data.Time as Time
import qualified Data.Time.Clock as Time
import Data.Tuple.Extra
import Piece.App.Env (TokenEnv (bSelectionToken))
import qualified Piece.App.Env as Env
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Db.Db as DB
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Role as Role
import qualified Piece.Db.User as User
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe

createNow :: (Time.MonadTime m) => m Token.Token
createNow = do
  now <- Unsafe.fromJust . rightToMaybe <$> Time.currentTime
  return $ Token.token 0 now

lookup :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Token.Token))
lookup = do
  tokenEnv <- Has.grab @Env.TokenEnv
  return $ flip Db.lookup <$> Env.bDatabaseToken tokenEnv

getTime :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Time.Time))
getTime = do
  bLookup <- lookup
  return $ (fmap Token.time .) <$> bLookup

getUserId :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe Db.DatabaseKey))
getUserId = do
  bLookup <- lookup
  return $ (fmap Token.user .) <$> bLookup

getUser :: (Env.WithTokenEnv env m, Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe User.User))
getUser = do
  bUserId <- getUserId
  bLookupUser <- User.lookup
  return $ (<=<) <$> bLookupUser <*> bUserId

getRoleIds :: (Env.WithTokenEnv env m, Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Db.DatabaseKey]))
getRoleIds = do
  bGetUser <- getUser
  return $ (fmap User.roles .) <$> bGetUser

getRoles :: (Env.WithTokenEnv env m, Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Role.Role]))
getRoles = do
  bGetRoles <- getRoleIds
  bLookupRole <- Role.lookup
  return $ (<=<) . mapM <$> bLookupRole <*> bGetRoles

getPrivilege :: (Env.WithTokenEnv env m, Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Db.DatabaseKey]))
getPrivilege = do
  bGetRoles <- getRoles
  return $ (fmap (>>= Role.privilege) .) <$> bGetRoles

lessThanTTL :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.NominalDiffTime -> Bool))
lessThanTTL = do
  tokenEnv <- Has.grab @Env.TokenEnv
  let bTTL = Env.bTTL tokenEnv
  return $ maybe (const False) (>) <$> bTTL

diffTime :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.Time -> Db.DatabaseKey -> Maybe (Time.Time, Db.DatabaseKey)))
diffTime = do
  bTokenTime <- getTime
  bLessThanTTL <- lessThanTTL
  return $ (\f -> curry . guarded . lmap (secondM f) . maybe False . lmap (uncurry Time.diffTime)) <$> bTokenTime <*> bLessThanTTL

validate :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.Time -> Maybe (Time.Time, DB.DatabaseKey)))
validate = do
  bDiffTime <- diffTime
  tokenEnv <- Has.grab @Env.TokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv
  return $
    (\diffTime mtoken now -> mtoken >>= \token -> diffTime now token)
      <$> bDiffTime
      <*> bSelectionToken

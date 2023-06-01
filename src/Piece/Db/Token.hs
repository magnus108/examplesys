module Piece.Db.Token
  ( lookup,
    getTime,
    isTokenUser,
    bOtherUsers,
    bOtherUsersFilter,
    getUserId,
    getUser,
    getRoleIds,
    getRoles,
    getPrivilege,
    getPrivilegeIds,
    lessThanTTL,
    createNow,
    validate,
    availableSelection,
  )
where

import Data.Profunctor
import qualified Data.Time as Time
import Data.Tuple.Extra
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

getRoleIds :: (Env.WithTokenEnv env m, Env.WithUserEnv env m) => m (R.Behavior (Db.DatabaseKey -> [Db.DatabaseKey]))
getRoleIds = do
  bGetUser <- getUser
  return $ (maybe [0] User.roles .) <$> bGetUser

getRoles :: (Env.WithTokenEnv env m, Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Role.Role]))
getRoles = do
  bGetRoles <- getRoleIds
  bLookupRole <- Role.lookup
  return $
    (\f g -> lmap g (mapM f))
      <$> bLookupRole
      <*> bGetRoles

getPrivilegeIds :: (Env.WithTokenEnv env m, Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Db.DatabaseKey]))
getPrivilegeIds = do
  bGetRoles <- getRoles
  return $ (fmap (>>= Role.privilege) .) <$> bGetRoles

getPrivilege :: (Env.WithPrivilegeEnv env m, Env.WithTokenEnv env m, Env.WithUserEnv env m, Env.WithRoleEnv env m) => m (R.Behavior (Db.DatabaseKey -> Maybe [Privilege.Privilege]))
getPrivilege = do
  bGetPrivilege <- getPrivilegeIds
  bLookupPrivilege <- Privilege.lookup
  return $ (<=<) . mapM <$> bLookupPrivilege <*> bGetPrivilege

lessThanTTL :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.NominalDiffTime -> Bool))
lessThanTTL = do
  tokenEnv <- Has.grab @Env.TokenEnv
  let bTTL = Env.bTTL tokenEnv
  return $ maybe (const False) (>) <$> bTTL

diffTime :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.Time -> Time.Time -> Bool))
diffTime = do
  bLessThanTTL <- lessThanTTL
  return $ curry . lmap (uncurry Time.diffTime) <$> bLessThanTTL

tokenDiffTime :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.Time -> Db.DatabaseKey -> Maybe Db.DatabaseKey))
tokenDiffTime = do
  bTokenTime <- getTime
  bDiffTime <- diffTime
  return $ (\f -> curry . (fmap snd .) . guarded . lmap (secondM f) . maybe True . uncurry) <$> bTokenTime <*> bDiffTime

validate :: (MonadIO m, Env.WithTokenEnv env m) => m (R.Behavior (Time.Time -> Maybe DB.DatabaseKey))
validate = do
  bTokenDiffTime <- tokenDiffTime
  tokenEnv <- Has.grab @Env.TokenEnv
  let bSelectionToken = Env.bSelectionToken tokenEnv
  return $
    (\diffTime mtoken now -> mtoken >>= \token -> diffTime now token)
      <$> bTokenDiffTime
      <*> bSelectionToken

isTokenUser :: (Env.WithTokenEnv env m) => m (R.Behavior (Db.DatabaseKey -> Bool))
isTokenUser = do
  tokenEnv <- Has.grab @Env.TokenEnv
  bUserId <- getUserId
  let bSelectionToken = Env.bSelectionToken tokenEnv
  return $ (\f i -> maybe (const False) (==) (f =<< i)) <$> bUserId <*> bSelectionToken

bOtherUsers :: (Env.WithTokenEnv env m, Env.WithUserEnv env m) => m (R.Behavior [Db.DatabaseKey])
bOtherUsers = do
  bUsers <- User.bListBox
  bIsTokenUser <- isTokenUser
  return $ (\p -> filter (not . p)) <$> bIsTokenUser <*> bUsers

bOtherUsersFilter :: (Env.WithTokenEnv env m, Env.WithUserEnv env m) => R.Behavior (String -> Bool) -> m (R.Behavior [Db.DatabaseKey])
bOtherUsersFilter bFilter = do
  bShowUser <- User.showUser
  bUsers <- bOtherUsers
  return $ (\p display -> filter (p . display)) <$> bFilter <*> bShowUser <*> bUsers

availableSelection :: (Env.WithUserEnv env m, Env.WithTokenEnv env m) => R.Behavior (Maybe Db.DatabaseKey) -> R.Behavior (String -> Bool) -> m (R.Behavior Bool)
availableSelection bSelection bFilter = do
  userEnv <- Has.grab @UserEnv.UserEnv
  otherUsers <- bOtherUsersFilter bFilter
  return $ maybe False . flip elem <$> otherUsers <*> bSelection

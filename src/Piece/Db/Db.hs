module Piece.Db.Db
  ( Database,
    DatabaseKey,
    emptydb,
    readJson,
    writeJson,
    lookup,
    keys,
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Relude.Unsafe as Unsafe

type DatabaseKey = Int

data Database a = Database {nextKey :: !Int, db :: M.Map DatabaseKey a}
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON)

emptydb :: Database a
emptydb = Database 0 mempty

keys :: Database a -> [DatabaseKey]
keys = M.keys . db

elems :: Database a -> [a]
elems = M.elems . db

toList :: Database a -> [(DatabaseKey, a)]
toList = M.toList . db

create :: a -> Database a -> Database a
create x (Database newkey db) = Database (newkey + 1) $ M.insert newkey x db

update :: DatabaseKey -> a -> Database a -> Database a
update key x (Database newkey db) = Database newkey $ M.insert key x db

delete :: DatabaseKey -> Database a -> Database a
delete key (Database newkey db) = Database newkey $ M.delete key db

lookup :: DatabaseKey -> Database a -> Maybe a
lookup key (Database _ db) = M.lookup key db

readJson :: (MonadIO m, FromJSON a) => FilePath -> m a
readJson fp = liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

writeJson :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJson fp items = liftIO $ BS.writeFile fp $ toStrict $ encode items

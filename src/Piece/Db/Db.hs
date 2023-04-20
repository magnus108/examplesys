module Piece.Db.Db
  ( Database,
    DatabaseKey,
    elems,
    create,
    empty,
    readJson,
    readJson2,
    writeJson,
    writeJson2,
    lookup,
    keys,
    toList,
    update,
  )
where

import Control.Exception (try)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as M
import GHC.IO.Exception (IOError)
import qualified Relude.Unsafe as Unsafe
import Prelude hiding (empty, toList)

type DatabaseKey = Int

data Database a = Database {nextKey :: !Int, db :: M.Map DatabaseKey a}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)

empty :: Database a
empty = Database 0 mempty

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
lookup key x = M.lookup key (db x)

readJson :: (MonadIO m, FromJSON a) => FilePath -> m a
readJson fp = liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

readJson_ :: FromJSON a => FilePath -> IO a
readJson_ fp = Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

readJson2 :: (MonadIO m, FromJSON a) => FilePath -> m (Either IOError a)
readJson2 fp = liftIO $ try (readJson_ fp)

writeJson :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJson fp items = liftIO $ BS.writeFile fp $ toStrict $ encode items

writeJson_ :: ToJSON a => FilePath -> a -> IO ()
writeJson_ fp items = BS.writeFile fp $ toStrict $ encode items

writeJson2 :: (MonadIO m, ToJSON a) => FilePath -> a -> m (Either IOError ())
writeJson2 fp items = liftIO $ try $ writeJson_ fp items

module Piece.Db.Db where

import Data.Aeson
import qualified Data.Map as M

type DatabaseKey = Int

data Database a = Database {nextKey :: !Int, db :: M.Map DatabaseKey a}
  deriving stock (Show, Generic)
  deriving stock (FromJSON, ToJSON)

emptydb = Database 0 mempty

keys = M.keys . db

elems = M.elems . db

toPairs = M.toPairs . db

create x (Database newkey db) = Database (newkey + 1) $ M.insert newkey x db

update key x (Database newkey db) = Database newkey $ M.insert key x db

delete key (Database newkey db) = Database newkey $ M.delete key db

lookup key (Database _ db) = M.lookup key db

readJson :: (MonadIO m, FromJSON a) => FilePath -> m a
readJson fp = liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

writeJson :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJson fp items = liftIO $ BS.writeFile fp $ toStrict $ encode items

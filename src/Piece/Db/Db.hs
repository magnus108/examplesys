module Piece.Db.Db
  ( Database,
    DatabaseKey,
    elems,
    create,
    empty,
    delete,
    lookup,
    keys,
    toList,
    update,
  )
where

import Data.Aeson
import qualified Data.Map as M
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

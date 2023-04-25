module Piece.Config
  ( Config (..),
    load,
  )
where

data Config = Config
  { datastoreLoan :: FilePath,
    datastoreUser :: FilePath,
    datastoreRole :: FilePath,
    datastorePrivilege :: FilePath,
    datastoreToken :: FilePath,
    datastoreItem :: FilePath,
    datastoreTab :: FilePath
  }

load :: Monad m => m Config
load =
  return $
    Config
      { datastoreLoan = "./data/loan.json",
        datastoreUser = "./data/user.json",
        datastoreRole = "./data/role.json",
        datastorePrivilege = "./data/privilege.json",
        datastoreToken = "./data/token.json",
        datastoreItem = "./data/item.json",
        datastoreTab = "./data/tab.json"
      }

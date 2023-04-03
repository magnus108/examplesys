module Piece.Config where

data Config = Config
    { datastoreLoan :: FilePath
    , datastoreUser :: FilePath
    , datastoreItem :: FilePath
    }

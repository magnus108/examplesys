module Piece
    ( mkAppEnv
    , runServer
    , main
    ) where

import Piece.App.Env (Env (..))
import Piece.App.Monad (AppEnv, runApp)


mkAppEnv :: IO AppEnv
mkAppEnv = do
    let envLogAction = mainLogAction Debug
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env = runApp env prepareDb >> run 8080 application
  where
    application = serve (Proxy @PieceApi) (server env)

main :: IO ()
main = mkAppEnv >>= runServer

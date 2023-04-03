module Piece
  ( mkAppEnv,
    runServer,
    main,
  )
where

import Piece.App.Env (Env (..))
import Piece.App.Monad (AppEnv, runApp)

mkAppEnv :: IO AppEnv
mkAppEnv = do
  -- let envLogAction = mainLogAction Debug
  -- pure Env{..}
  undefined

runServer :: AppEnv -> IO ()
runServer env = undefined -- runApp env prepareDb >> run 8080 application
-- where
-- application = serve (Proxy @PieceApi) (server env)

main :: IO ()
main = undefined -- mkAppEnv >>= runServer

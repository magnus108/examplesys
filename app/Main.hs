module Main (main) where

import qualified Piece2
import Text.Read

main :: IO ()
main = do
  [port] <- getArgs
  Piece2.main (read port)

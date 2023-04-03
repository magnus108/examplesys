module Main (main) where

import qualified Piece
import           Text.Read

main :: IO ()
main = do
	[port] <- getArgs
	Piece.main (read port)

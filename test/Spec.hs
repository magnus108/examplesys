module Main
  ( main,
  )
where

import qualified Piece.Core.Item.Tests
import qualified Piece.Db.Db.Tests
import qualified Piece.Db.Loan.Tests
import qualified Piece.Db.Tab.Tests
import qualified Piece.Db.Token.Tests
import qualified Piece.Db.User.Tests
import qualified Piece.Effects.Read.Tests
import qualified Piece.Effects.Write.Tests
import qualified Piece.Gui.Loan.Behavior.Tests
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Piece.Core.Item.Tests.tests,
        Piece.Db.Loan.Tests.tests,
        Piece.Db.Token.Tests.tests,
        Piece.Db.Tab.Tests.tests,
        Piece.Db.User.Tests.tests,
        Piece.Effects.Write.Tests.tests,
        Piece.Effects.Read.Tests.tests,
        Piece.Gui.Loan.Behavior.Tests.tests,
        Piece.Db.Db.Tests.tests
      ]

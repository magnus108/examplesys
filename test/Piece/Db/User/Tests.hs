{-# LANGUAGE DataKinds #-}

module Piece.Db.User.Tests
  ( tests,
  )
where

import Data.Text (pack)
import qualified Data.Time as Time
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.Time as Time
import qualified Piece.Core.Token as Token
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Token as Token
import qualified Piece.Db.User as User
import qualified Piece.Effects.Time as Time
import qualified Reactive.Threepenny as R
import qualified Relude.Unsafe as Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Mock
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Piece.Db.User.Tests" $
    concat
      [ fromAssertions
          "bListBox"
          [ runMockApp $ do
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              user <- User.create $ UserCreateForm.form "user1" "pass1" False
              liftIO $ hUser user
              user <- User.create $ UserCreateForm.form "user2" "pass2" False
              liftIO $ hUser user
              user <- User.create $ UserCreateForm.form "user3" "pass3" False
              liftIO $ hUser user
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              bListBox <- User.bListBox
              currentTokenKey <- R.currentValue bListBox
              let value = [0, 1, 2]
              liftIO $ value @=? currentTokenKey
          ]
      ]

{-# LANGUAGE DataKinds #-}

module Piece.Db.Tab.Tests
  ( tests,
  )
where

import Data.Text (pack)
import qualified Data.Time as Time
import Piece.App.Env (PrivilegeEnv (PrivilegeEnv))
import qualified Piece.CakeSlayer.Has as Has
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.Privilege as Privilege
import qualified Piece.Core.Role as Role
import qualified Piece.Core.Tab as Tab
import qualified Piece.Core.Time as Time
import qualified Piece.Core.User as User
import qualified Piece.Core.UserCreateForm as UserCreateForm
import qualified Piece.Db.Db as Db
import qualified Piece.Db.Tab as Tab
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
  testGroup "Piece.Db.Tab.Tests" $
    concat
      [ fromAssertions
          "lookup"
          [ runMockApp $ do
              hTab <- Has.grab @(R.Handler Tab.Tab)
              liftIO $ hTab (Tab.tab "test" [])
              x <- Tab.lookup
              currentTab <- R.currentValue (x ?? 0)
              let value = Just "test"
              liftIO $ value @=? (Tab.name <$> currentTab)
          ],
        fromAssertions
          "getName"
          [ runMockApp $ do
              hTab <- Has.grab @(R.Handler Tab.Tab)
              liftIO $ hTab (Tab.tab "test" [])
              x <- Tab.getName
              currentTab <- R.currentValue (x ?? 0)
              let value = Just "test"
              liftIO $ value @=? currentTab
          ],
        fromAssertions
          "getPrivilege"
          [ runMockApp $ do
              hTab <- Has.grab @(R.Handler Tab.Tab)
              liftIO $ hTab (Tab.tab "test" [])
              x <- Tab.getPrivilegeIds
              currentTab <- R.currentValue (x ?? 0)
              let value = Just []
              liftIO $ value @=? currentTab,
            runMockApp $ do
              hTab <- Has.grab @(R.Handler Tab.Tab)
              liftIO $ hTab (Tab.tab "test" [1, 2])
              x <- Tab.getPrivilegeIds
              currentTab <- R.currentValue (x ?? 0)
              let value = Just [1, 2]
              liftIO $ value @=? currentTab
          ],
        fromAssertions
          "listBox"
          [ runMockApp $ do
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)
              hTab <- Has.grab @(R.Handler Tab.Tab)
              liftIO $ hTab (Tab.tab "test" [0, 1])
              liftIO $ hTab (Tab.tab "test1" [0, 1])
              liftIO $ hTab (Tab.tab "test2" [0, 1])

              hPrivilege <- Has.grab @(R.Handler Privilege.Privilege)
              liftIO $ hPrivilege (Privilege.privilege "test1")
              liftIO $ hPrivilege (Privilege.privilege "test2")

              hRole <- Has.grab @(R.Handler Role.Role)
              liftIO $ hRole (Role.role "test" [0, 1])

              x <- Tab.listBox
              currentTabs <- R.currentValue x
              let value = []
              liftIO $ value @=? currentTabs,
            runMockApp $ do
              user <- User.create $ UserCreateForm.user "lol" (Password.PasswordPlainText (pack "lol")) True
              hUser <- Has.grab @(R.Handler (Maybe User.User))
              liftIO $ hUser user
              hUserLogin <- Has.grab @(R.Handler (Maybe Db.DatabaseKey))
              liftIO $ hUserLogin (Just 0)

              hPrivilege <- Has.grab @(R.Handler Privilege.Privilege)
              liftIO $ hPrivilege (Privilege.privilege "test1")
              liftIO $ hPrivilege (Privilege.privilege "test2")

              hRole <- Has.grab @(R.Handler Role.Role)
              liftIO $ hRole (Role.role "test" [0, 1])
              liftIO $ hRole (Role.role "test" [0, 1])

              hTab <- Has.grab @(R.Handler Tab.Tab)
              liftIO $ hTab (Tab.tab "test" [0, 1])
              liftIO $ hTab (Tab.tab "test1" [0, 1])
              liftIO $ hTab (Tab.tab "test2" [0, 1])

              x <- Tab.listBox
              currentTabs <- R.currentValue x
              let value = [0, 1, 2]
              liftIO $ value @=? currentTabs
          ]
      ]
